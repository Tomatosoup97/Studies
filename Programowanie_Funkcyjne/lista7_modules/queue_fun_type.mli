module type QUEUE_FUN =
sig
  (* Module [QueueFun]: first-in first-out queues *)

  (* This module implements queues (FIFOs)in a functional way. *)

  type 'a t
        (* The type of queues containing elements of type ['a]. *)
  exception Empty of string
        (* Raised when [first] is applied to an empty queue. *)
  val create: unit -> 'a t
        (* Return a new queue, initially empty. *)
  val enqueue: 'a * 'a t -> 'a t
        (* [enqueue x q] adds the element [x] at the end of queue [q]. *)
  val dequeue: 'a t -> 'a t
        (* [dequeue q] removes the first element in queue [q] *)
  val first: 'a t -> 'a
        (* [first q] returns the first element in queue [q] without removing
           it from the queue, or raises [Empty] if the queue is empty.*)
  val isEmpty: 'a t -> bool
        (* [isEmpty q] returns [true] if queue [q] is empty,
           otherwise returns [false]. *)
end;;

