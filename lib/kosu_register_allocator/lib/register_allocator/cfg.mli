(**
    Cfg functor create multiples types to represent the control flow graph (Cfg) of the program

    {!S.Basic: Basic} contains the type [('a, 'b) basic_block] which represent a sequence of stamement without a jump or goto and 
      the type [cfg] which represents the control flow graph of a function by containing a set of [('a, 'b) basic_block]

    {!S.Detail: Detail} defines [cfg_detail] which extends [cfg] by adding for each basic_block the input variable and the output variable

    {!S.Liveness: Liveness} extends [cfg_detail] with the type [cfg_liveness] which add for each statement the liveness info of each variable

    {!S.Inference_Graph: Inference_Graph} compute the variable inference graph from the [cfg_liveness]

    {!S.GreedyColoring: GreedyColoring} is a functor which try to color the infered graph with type [color] defined in the functor {!S.ColoredType: ColoredType}.
    The coloration algorithm used is roughly the Kempe algorithm
*)

(** Input signature of of the functor {!Make}*)
module type CfgS = sig
  type variable
  (** the type of variable *)

  type t = variable
  (** an alias of variable of compatiblity with other ocaml functor *)

  type atom
  (** the type of elementary value in a 3 address code language *)

  type rvalue
  (** the type of "expression" at the right in a 3 address code langauge  *)

  val repr : variable -> string
  (** string representation of [varriable]: Mostly for debug *)

  val compare : variable -> variable -> int
  (** compare two variable*)

  (* Create a variable from the lvalue identifier and the [rvalue] *)
  val lvalue_variable : string -> rvalue -> variable
  val lvalue_deref_variable : string -> rvalue -> variable
  val ttrv_identifiers_used : rvalue -> variable list
  val tte_idenfier_used : atom -> variable list

  val is_affectation : rvalue -> bool
  (** return whenever the rvalue affected a value to the variable *)

  val variables_as_parameter : rvalue -> (variable * int) list option
end

module type CfgPprintSig = sig
  type variable
  type rvalue
  type atom

  val string_of_variable : variable -> string
  val string_of_rvalue : rvalue -> string
  val string_of_atom : atom -> string
end

module type ABI = sig
  type t
  type variable

  type return_strategy =
    | Indirect_return
    | Simple_return of t
    | Splitted_return of t * t

  val compare : t -> t -> int
  val callee_saved_register : t list
  val caller_saved_register : t list
  val syscall_register : t list

  val arguments_register : variable -> t list
  (** 
      Select the argument registers list used for [variable]
      Can be used for example to distinguish float arguments
    *)

  val non_float_argument_registers : t list
  val is_valid_register : variable -> t -> bool
  val does_return_hold_in_register : variable -> bool
  val indirect_return_register : t
  val return_strategy : variable -> return_strategy
end

module type ColoredType = Graph.ColoredType

module type VariableSig = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type variable
  type rvalue
  type atom
  type constraints

  module VariableSig : sig
    include VariableSig with type t = variable
  end

  module StringSet : sig
    include module type of Set.Make (String)
  end

  module TypedIdentifierSet : sig
    include module type of Set.Make (VariableSig)
  end

  module BasicBlockMap : sig
    include Map.S with type key = String.t
  end

  type cfg_statement =
    | CFG_STacDeclaration of { identifier : string; trvalue : rvalue }
    | CFG_STacModification of { identifier : string; trvalue : rvalue }
    | CFG_STDerefAffectation of { identifier : string; trvalue : rvalue }

  type bbe_if = { condition : atom; if_label : string; else_label : string }
  type basic_block_end = BBe_if of bbe_if | Bbe_return of atom

  module Basic : sig
    type ('a, 'b) basic_block = {
      label : string;
      cfg_statements : 'a list;
      followed_by : StringSet.t;
      ending : 'b;
    }

    type cfg = {
      entry_block : string;
      blocks :
        (cfg_statement, basic_block_end option) basic_block BasicBlockMap.t;
      parameters : variable list;
      locals_vars : TypedIdentifierSet.t;
    }

    val create_basic_block :
      label:string ->
      cfg_statements:'a list ->
      followed_by:string list ->
      ending:'b ->
      ('a, 'b) basic_block

    val create_cfg :
      entry_block:string ->
      parameters:variable list ->
      locals_vars:variable list ->
      (cfg_statement, basic_block_end option) basic_block list ->
      cfg
  end

  module Detail : sig
    type ('a, 'b) basic_block_detail = {
      basic_block : ('a, 'b) Basic.basic_block;
      in_vars : TypedIdentifierSet.t;
      out_vars : TypedIdentifierSet.t;
    }

    type cfg_detail = {
      entry_block : string;
      blocks_details :
        (cfg_statement, basic_block_end option) basic_block_detail
        BasicBlockMap.t;
      parameters : variable list;
      locals_vars : TypedIdentifierSet.t;
    }

    val of_cfg : Basic.cfg -> cfg_detail
  end

  module Liveness : sig
    module LivenessInfo : sig
      include LivenessInfo.LivenessInfoS with type elt = variable
    end

    type cfg_liveness_statement = {
      cfg_statement : cfg_statement;
      liveness_info : LivenessInfo.liveness_info;
    }

    type liveness_ending = basic_block_end option * LivenessInfo.liveness_info

    type cfg_liveness_detail = {
      entry_block : string;
      blocks_liveness_details :
        (cfg_liveness_statement, liveness_ending) Detail.basic_block_detail
        BasicBlockMap.t;
      parameters : variable list;
      locals_vars : TypedIdentifierSet.t;
    }

    val of_cfg_details :
      delete_useless_stmt:bool -> Detail.cfg_detail -> cfg_liveness_detail
  end

  module Interference_Graph : sig
    module IG : sig
      include module type of Graph.Make (VariableSig)
    end

    val interfere : Liveness.cfg_liveness_detail -> IG.graph
  end

  module GreedyColoring (ABI : ABI with type variable = VariableSig.t) : sig
    module ColoredGraph : sig
      include module type of Graph.ColoredMake (VariableSig) (ABI)
    end

    val coloration :
      parameters:(variable * ABI.t) list ->
      available_color:ABI.t list ->
      Liveness.cfg_liveness_detail ->
      ColoredGraph.colored_graph
  end
end

module type SP = sig
  include S

  module Pprint : sig
    val string_of_typed_indentifier_set : TypedIdentifierSet.t -> string
    val string_of_cfg_statement : cfg_statement -> string

    val string_of_cfg_liveness_statement :
      Liveness.cfg_liveness_statement -> string

    val string_of_basic_block_end : basic_block_end -> string
  end
end

module Make (CfgS : CfgS) :
  S
    with type variable = CfgS.variable
     and type atom = CfgS.atom
     and type rvalue = CfgS.rvalue

module MakePprint
    (CfgS : CfgS)
    (_ : CfgPprintSig
           with type variable = CfgS.variable
            and type atom = CfgS.atom
            and type rvalue = CfgS.rvalue) :
  SP
    with type variable = CfgS.variable
     and type atom = CfgS.atom
     and type rvalue = CfgS.rvalue
