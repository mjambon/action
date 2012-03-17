type region_id = int

module type In =
sig
  type element_contents
  type region_contents
end
module type Out =
sig
  type element_contents
  type region_contents

  type region = {
    id: region_id;
    children: (region * region) option;
    data: region_contents;
  }

  type t
  type elt

  val create : 
    diffusion_period: int ->
    fraction_diffused: float ->
    post_move: (region -> region -> elt -> unit) ->
    region -> t

  val elt_contents : elt -> element_contents

  val add_element : t -> region_id -> element_contents -> float -> elt
    (** Create an element and add it to the tree.
        Once added, an element cannot be removed from the tree. *)

  val update_score : elt -> float -> unit
    (** Update the score of an element,
        possibly moving it to another region. *)

  val iter_region : t -> region_id -> (elt -> unit) -> unit
  val fold_region : region -> 'acc -> (elt -> 'acc -> 'acc) -> 'acc
end

module Make (Param : In) : Out
  with type element_contents = Param.element_contents
  and type region_contents = Param.region_contents
