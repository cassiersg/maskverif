type ty = W1 | W8 | W16 | W32 | W64

val w1    : ty 
val w8    : ty
val w16   : ty
val w32   : ty
val w64   : ty

val ty2string : ty -> string 

type var = private {
  v_id   : int;
  v_name : string;
  v_ty   : ty;
}

module V : sig
  val mk_var  : string -> ty -> var
  val clone   : var -> var
  val equal   : var -> var -> bool
  val compare : var -> var -> int
end

module Sv : Set.S with type elt = var
module Mv : Map.S with type key = var
module Hv : Hashtbl.S with type key = var

type rnd = var

type param = var

type op_kind = Add | Mul | Neg | Other

type operator = private {
  op_id    : int;
  op_name  : string;
  op_ty    : (ty list * ty) option;
  op_bij   : bool; (* true means  bijective *)
  op_kind  : op_kind
}

module Op : sig
          
  val make : string -> (ty list * ty) option -> bool -> op_kind -> operator
  val find : string -> operator
  val equal: operator -> operator -> bool
  val hash : operator -> int 
  
end

val o_addb   : operator
val o_addw8  : operator
val o_addw16 : operator
val o_addw32 : operator
val o_addw64 : operator
val o_mulb   : operator
val o_mulw8  : operator
val o_mulw16 : operator
val o_mulw32 : operator
val o_mulw64 : operator
val o_negb   : operator
val o_negw8  : operator
val o_negw16 : operator
val o_negw32 : operator
val o_negw64 : operator

val o_mul2: operator
val o_mul3: operator
val o_mulc: operator
val o_affaes: operator
val o_addpub: operator
val o_square: operator

val _DFF_P_     : operator
val _DFF_PP0_   : operator
val _DFFSR_PPP_ : operator
val _DFF_PN0_   : operator

val is_FF_op : operator -> bool 

type constant = private {
    c_ty : ty;
    c_val : Z.t;
  }

module C : sig 

  type t = constant
  val equal : t -> t -> bool 
  val hash  : t -> int 
  val pp :  Format.formatter -> t -> unit 
  val make : ty -> Z.t -> t
  val _true : t 
  val _false : t

end

type expr = private {
  e_id : int;
  e_node : expr_node
}

and expr_node =
| Etop
| Ernd   of rnd
| Eshare of param * int * var (* the var is the original name *)
| Epub   of var
| Eop1   of operator * expr
| Eop2   of operator * expr * expr
| Eop    of bool * operator * expr array
  (* Invariant [Eop(b,es)]
       if b is true there is no duplicate in the array es *)
| Econst of constant

module E : sig
  val mk_expr : expr_node -> expr
  val equal : expr -> expr -> bool
  val hash  : expr -> int
  val compare : expr -> expr -> int
end

module He : Hashtbl.S with type key = expr

module Se : Set.S with type elt = expr

module Me : Map.S with type key = expr

val type_of_expr : expr -> ty option

val top   : expr
val rnd   : rnd -> expr
val share : param -> int -> var -> expr
val pub   : var -> expr
val econst : constant -> expr
val efalse : expr
val etrue  : expr
val eone  : ty -> expr
val ezero : ty -> expr
val op1   : operator -> expr -> expr
val op2   : operator -> expr -> expr -> expr
val o_add : ty -> operator
val o_mul : ty -> operator
val o_neg : ty -> operator

val neg   : expr -> expr
val add   : expr -> expr -> expr
val mul   : expr -> expr -> expr
val op    : operator -> expr array -> expr
val tuple : expr array -> expr

val mul2: expr -> expr
val mul3: expr -> expr
val mulc: expr -> expr
val affaes: expr -> expr
val addpub: expr -> expr
val square: expr -> expr

val is_op_tuple : operator -> bool
val is_rnd      : expr -> bool


(* [unsafe_op b es]: [b] should be true only if [es] has no duplicate *)
val unsafe_op : bool -> operator -> expr array -> expr

(* [tuple_nodup es]: [es] should not contains duplicate *)
val tuple_nodup : expr array -> expr
val tuple : expr array -> expr


val pp_var    : Format.formatter -> var -> unit
val pp_rnd    : Format.formatter -> rnd -> unit
val pp_share  : Format.formatter -> param * int * var -> unit

type top = [ `Top |  `AddL | `AddR | `MulL | `MulR | `Not]

val pp_op1 : 
  (top -> 'a Util.pp) -> 
  Format.formatter -> operator -> 'a -> unit

val pp_op2 : 
  (top -> 'a Util.pp) -> 
  Format.formatter -> top -> operator -> 'a -> 'a -> unit
              
val pp_op :
  (top -> 'a Util.pp) ->  
  Format.formatter -> operator -> 'a array -> unit

val pp_expr : expr Util.pp

