## Properties of the BST
- Search, Insert, Delete supported
- Search explores the tree and returns true iff the key is present
- Insert adds a key to the tree if it is not already present
- Delete removes a key if it is present.
- Both Insert and Delete return true if the key set is changed, and false otherwise
- Duplicate keys are not allowed
- Left Subtree : only nodes with smaller keys than current node
- Right Subtree : only nodes with greater than or equal to keys with current node
- Left and Right subtrees are also BSTs
- Every internal node has exactly two children

## Overview of Algorithm
- Seek Phase : Traverse the search tree starting from root node to leaf node.
- Access path : Path traversed in the seek phase
- grandparent, parent, leaf nodes : The last three nodes in the access path
- All operations perform a seek phase in the beginning

### Search
- Perform Seek with a key. If the key of the leaf node matches, return true. Else, return false.

### Insert 
- Perform seek with a key. If the key of the leaf node matches, return false, because the key already exists in the tree.
- If the key does not match, we move to the execution phase.
- Insert by adding two nodes, a new internal node, and a new leaf node.
- The children of the new internal nodes, are the leaf node from seek, and the inserted leaf node.

### Delete
- Perform seek with a key. If the key of the leaf node does not match, return false. 
- If it does match, we move to the execution phase.
- Remove the node by removing the leaf and parent nodes on the access path.
- Then change the pointer of the grandparent node to point to the sibling of the leaf node.

- Injection point: The parent node on the access path.

## Details of the Algorithm
- We are assuming no memory reclamation is done. If we want to reclaim deleted nodes, look into Hazard Pointers (Ref 26 in the paper)
- The algorithm works by operating on the edge level, obtaining ownership of the edges(by marking them) it has to work on, not the nodes.

### Marking an edge
- Marking an edge means that either its tail and head, or only its tail will be removed from the tree. 
- Flagging: marking an edge for which tail and head are to be removed.
- Tagging: marking an edge for which only tail has to be removed.
- Once an edge is marked, it cannot be changed, meaning it can't point to another node. So, once both the child edges of a node are marked, it cannot be used as an injection point of another operation. This is because, once both its child edges are marked, none of its children nodes can be changed.
 To enable flagging and tagging, we steal two bits from each child address, *flag* and *tag*

### Seek Phase
- The seek phase return a _seek record_, which consists of the addresses of four nodes:
    - *leaf* node
    - *parent* node
    - *successor* node : The head of the last untagged edge encountered on the access path before the parent node
    - *ancestor* node : The tail of the last untagged edge encountered on the access path before the parent node
- All the nodes on the access path from the successor to the parent node are in the process of being removed

#### Sentinel nodes
- To ensure that all nodes are well-defined at all times, we assume the presence of three sentinel keys, S0, S1, S2.
- All other keys < S0 < S1 < S2.
- Sentinels are never removed from the tree.
- Since the sentinels are greater than all other nodes, we can guarantee that all inserted keys will be in the left subtree of the left child of the root node.

#### Seek procedure
- _ancestor_ is initialized to the root node
- _successor_ and _parent_ are initialized to its left child
- _leaf_ is initialized to the left child of the left child
- While the path can still be extended (while the _leaf_ field is not a leaf node) 
    - If the current edge is untagged, advance the _ancestor_ and _successor_
    - Find the next edge by looking at the keys, and advance the _leaf_ and _parent_

### Search operation
- Execute seek, and return true iff the key of _leaf_ is the same as the query

### Insert operation
- Execute seek, and return false if the key of _leaf_ is the same as the query
- Else, execution phase
- Let _k_ be the key to be inserted, and _k'_ be the key in _leaf_
- Create an internal node _newInternal_ and a leaf node _newLeaf_
- _newLeaf_._key_ = k; _newInternal_._key_ = max(_k_, _k'_)
- If _k_ < _k'_, then _newLeaf_ becomes the left child of _newInternal_, else the right child
- Set the other child of _newInternal_ to be _leaf_
- Then, try to replace the edge (_parent_, _leaf_) with  (_parent_, _newInternal_) using an atomicCAS.
- If it succeeds, the operation is complete, else perform **_helping_** if needed and restart
- It can fail only if the edge does not exist, or has been flagged.

#### Helping
- We check if the edge from _parent_ to _leaf_ still exists, and has been marked. If so, then there is a concurrent delete trying to remove _parent_
- In that case, the insert helps by performing the last two steps of the delete, which results in _parent_ and one of its children being removed from the tree.
- Do this by invoking the ***cleanup*** routine of the delete operation

### Delete Operation
- Execute seek, and return false if the key of _leaf_ is not the same as the query
- Else, there are two modes, the ***injection*** and the ***cleanup*** mode

#### Injection
- Objective is to locate the correct leaf node, and mark it by flagging it's incoming edge
- The leaf node is the _leaf_ from the seek phase
- Try flagging the (_parent_, _leaf_) edge with an atomicCAS instruction
- If it succeeds, the delete is guaranteed to complete eventually, enter ***cleanup*** mode.
- Else, perform ***helping*** if needed, and restart from the seek phase in the ***injection*** mode.
- We decide the need for ***helping*** in the same way, it is done only if the edge still exists and has been marked.
- ***Helping*** is done by invoking the ***cleanup*** routine

#### Cleanup
- Notice that when entering the cleanup stage, the edge from _parent_ to _leaf_ has always been flagged
- WLOG, assume _leaf_ is the left child of _parent_
- Tag(set the tag bit) the other child(right in this case) of _parent_ using a BTS instruction.
- This step is guaranteed to succeed. After this step, neither of _parent_'s children can be changed.
- Let _S_ denote the right child of _parent_. In the second step, we try to replace (_ancestor_, _successor_) with (_ancestor_, _S_) using an atomicCAS on _ancestor_'s appropriate child field.
- The edge (_parent_, _S_) might have been flagged by another delete operation trying to remove the key stored in _S_. In this case the edge must have been flagged before being tagged, because we never flag a tagged edge.
- The value of the _flag_ bit in the _right_ field of _parent_ is copied to the new edge from _ancestor_ to _S_, if _S_ is to be deleted, we won't lose that information in this operation.
- If the CAS succeeds, the delete is done. Else, execute the seek phase again repeat in the ***cleanup*** mode. This means, we don't have to flag the edge as we do in the ***injection*** mode
- The number of times this phase repeats is guaranteed to be finite because:
    - Once the edge to a leaf has been flagged, no new nodes can be inserted in the access path. This is because inserts only add nodes at the _leaf_ and _parent_ level, and if the _parent_ is marked, helping happens before the insertion.
    - So, every time the CAS fails, it is because the access path has lost one or more internal nodes, or the last untagged edge has moved closer to the root node.

### Summary
- The delete operations performs the seek phase in the ***injection*** mode first, and cycles between ***injection*** and helping until the CAS of the ***injection*** succeeds, then it cycles in the ***cleanup*** mode with a new seek record each time until the CAS of the ***cleanup*** succeeds

## Correctness

### Legality
- We can show that the traversal during the seek phase never takes a "wrong turn". This means that the injection point of a modify operation is always a "correct" node if it completes, so the BST is always legal.

### Linearizability
- Observations:
    - The key stored at a node, never changes once initialized
    - An internal node stays internal, and a leaf node stays a leaf node
    - Before an internal node is removed, both its child edges are marked, after which they cannot change. This implies that the first CAS of a modify op, succeeds only if its injection point is still part of the tree when the CAS is performed.
- Linearization points (LPs):
    - Modify operations that fail are treated as search operations.
    - The LP of an insert is the point at which it performs its CAS successfully.
    - The LP of a delete is the point at which it performs the CAS which removes the target key from the tree successfully. This instruction may also be performed by another helping operation.
    - If multiple keys are removed in the same CAS, the LPs of the corresponding deletes can be ordered arbitrarily since all delete ops must have distinct target keys.
    - For a search-hit operation, if the leaf node returned is still part of the tree when the search completes, then the LP is the end of the search phase. Else, the LP is the point just before the leaf node is removed from the tree
    - For a search-miss operation, if there are one or more delete operations on the same key with an LP overlapping the search, the point after the last such LP is taken to be the LP of the search. Else, the LP is the point at which the operation starts.

### Lock-Freedom
- If the system reaches a state after which no modify operation completes, then every search operation is guaranteed to complete after that because the tree will not undergo any more structural changes. So, it is enough to prove that modify operations are lock-free.
- Assume there is an operation _A_ that takes an infinite number of steps but still no modify operation completes after that.
- This means that there is a time _t_, after which the tree stops changing.
- Every instance of the seek phase of _A_ that starts after _t_ traverses the same access path and returns the same injection point. 
- The repeated failure of _A_ implies that there is a delete operation _B_ conflicting with _A_.
- Every time the process fails to inject _A_, it will try to help _B_.
- It can be verified that every time the process fails to help _B_, the depth of the ancestor pointer returned by the subsequent seek phase of _A_ will strictly decrease.
- Since depth can decrease only a finite number of times, _B_ is guaranteed to complete.
