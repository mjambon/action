type 'a t
  (** binary tree of regions *)

type region_id = int
    (** unique identifier of a region within a given tree.
        Given an input array of [n] base regions,
        the region IDs from 0 to [(n-1)] correspond to the
        indices in the array.
        Other region numbers are greater than [(n-1)]. *)

type 'a region_data
type 'a region = private {
  region_id : region_id;
  region_data : 'a region_data;
  region_tree : 'a t;
}
  (** a region, i.e. a node of the tree *)

type elt_data
type 'a elt = private {
  mutable elt_score : float;
  mutable elt_region : 'a region;
  elt_data : elt_data;
  elt_contents : 'a;
}
    (** an element within a region *)

val create : int -> 'a t
  (** [create n] creates a new binary tree with [n] base regions (leaves).
      [n] must be a power of two. *)

val add_element : 'a t -> region_id -> 'a -> float -> 'a elt
  (** Create an element and add it to the tree.
      Once added, an element cannot be removed from the tree. *)

val update_score : 'a elt -> float -> unit
  (** Update the score of an element, possibly moving it to another region. *)

val iter_tree : ('a region -> unit) -> 'a t -> unit
val fold_tree : ('a region -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

val iter_region : ('a elt -> unit) -> 'a region -> unit
val fold_region : ('a elt -> 'acc -> 'acc) -> 'a region -> 'acc -> 'acc
