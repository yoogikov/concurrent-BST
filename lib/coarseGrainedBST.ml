(* coarseGrainedBST.ml
 * 
 * coarse grained BST implementation stub.
 * 
 * 
 *)

type key = int

type 'a node = {
  key : key;
  value : 'a;
  left : 'a node option ref;
  right : 'a node option ref;
}



type 'a t = {
  mutex : Mutex.t;
  head : 'a node option ref;
  size : int;
}

let create () =
  {
    mutex = Mutex.create ();
    head = ref None;
    size = 0;
  }

let search bst _ =
  failwith "Not implemented"

let insert bst _ =
  failwith "Not implemented"

let delete bst _ =
  failwith "Not implemented"

let size bst =
  Mutex.lock bst.mutex;
  let sz = bst.size in
  Mutex.unlock bst.mutex;
  sz
