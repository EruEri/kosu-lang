module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type LivenessInfoS = sig
  type elt
  type liveness_info

  val init : (elt -> bool) -> elt list -> liveness_info
  val is_alive : elt -> liveness_info -> bool
  val is_dead : elt -> liveness_info -> bool
  val set_alive : elt -> liveness_info -> liveness_info
  val of_list : (elt * bool) list -> liveness_info
  val to_list : liveness_info -> (elt * bool) list
  val elements : liveness_info -> elt list
  val alive_elements : liveness_info -> elt list
  val set_dead : elt -> liveness_info -> liveness_info
  val set_dead_of_list : elt list -> liveness_info -> liveness_info
end

module Make (OrderedType : OrderedType) :
  LivenessInfoS with type elt = OrderedType.t
