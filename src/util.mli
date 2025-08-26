module List : sig
    include module type of List

    val count : ('a -> bool) -> 'a list -> int
    val map_size : ('a -> 'b) -> 'a list -> 'b list * int
    val is_empty : 'a list -> bool
    val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
    val split : int -> 'a list -> 'a list * 'a list
  end

val partition :
  ('a -> bool) -> 'a list -> 'a list -> 'a list -> 'a list * 'a list

val mk_range_i : int -> int -> int list
(* ----------------------------------------------------------------------- *)
type 'a pp = Format.formatter -> 'a -> unit

val pp_if    : bool -> 'a pp -> 'a pp -> 'a pp
val pp_maybe : bool -> ('a pp -> 'a pp) -> 'a pp -> 'a pp

val pp_enclose:
       pre:('a, 'b, 'c, 'd, 'd, 'a) format6
   -> post:('a, 'b, 'c, 'd, 'd, 'a) format6
   -> 'a pp -> 'a pp

val pp_paren : 'a pp -> 'a pp
val pp_maybe_paren : bool -> 'a pp -> 'a pp

val pp_list : ('a, 'b, 'c, 'd, 'd, 'a) format6 -> 'a pp -> 'a list pp


(* ----------------------------------------------------------------------- *)
module Sint : Set.S with type elt = int
module Mint : Map.S with type key = int
module Mstr : Map.S with type key = string

(* ----------------------------------------------------------------------- *)
module Array : sig
    include module type of Array

    val for_all : ('a -> bool) -> 'a array -> bool
    val for_all2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
  end

(* ----------------------------------------------------------------------- *)
exception EmptyStack

module Stack :
  sig
    type 'a t
    val make : int  -> 'a -> 'a t
    val push : 'a t -> 'a -> unit
    val pop  : 'a t -> 'a
    val clear: 'a t -> unit
    val copy : 'a t -> 'a t
    val to_list : 'a t -> 'a list
    val iter : ('a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val length : 'a t -> int
  end

(* ----------------------------------------------------------------------- *)
val finally : (unit -> unit) -> ('a -> 'b) -> 'a -> 'b

(* ----------------------------------------------------------------------- *)
module Vector : sig
  type 'a t

  val dummy  : unit -> 'a t
  val create : int -> 'a -> 'a t
  val copy   : 'a t -> 'a t
  val clear  : 'a t -> unit
  val get    : 'a t -> int -> 'a
  val push   : 'a t -> 'a -> unit
  val size   : 'a t -> int
  val pop    : 'a t -> 'a
  val top    : 'a t -> 'a

  (* remove all elements satisfying the predicate *)
  val remove : ('a -> bool) -> 'a t -> unit
  val unset  : 'a t -> int -> unit

  val iter   : ('a -> unit) -> 'a t -> unit
  val exists : ('a -> bool) -> 'a t -> bool

  val to_list : 'a t -> 'a list
end

(* ------------------------------------------------------------------------ *)

type location = {
  lc_fname : string;
  lc_start : int * int;
  lc_end   : int * int;
  lc_bchar : int;
  lc_echar : int;
}

(* -------------------------------------------------------------------- *)
type 'a located = { pl_data: 'a; pl_location: location; }

val mkloc : location -> 'a -> 'a located

val loc  : 'a located -> location
val data : 'a located -> 'a

(* -------------------------------------------------------------------- *)

module Location : sig
  open Lexing

  type t = location

  val make      : position -> position -> t
  val of_lexbuf : lexbuf -> t
  val to_string : t -> string
end

val warning :
  ?loc:Location.t -> ('a, Format.formatter, unit, unit) format4 -> 'a


exception Error of (string * Location.t option * string)

val error :
  string -> Location.t option -> ('a, Format.formatter, unit, 'b) format4 -> 'a

val pp_error :
  Format.formatter -> string * Location.t option * string -> unit

(* -------------------------------------------------------------------- *)
exception ParseError of Location.t * string option
exception LexicalError of Location.t option * string
(* -------------------------------------------------------------------- *)

type hstring = private {
    hs_id : int;
    hs_str : string;
  }

module HS : sig

  val make : string -> hstring

  type t = hstring

  val equal : t -> t -> bool

  val hash : t -> int

  val compare : t -> t -> int

  val pp : bool -> t pp  
end

(* --------------------------------------------------------- *)
val pp_z : Format.formatter -> Z.t -> unit
val pp_human : string -> Format.formatter -> Z.t -> unit

(* --------------------------------------------------------- *)
val fverbose :
  int -> Format.formatter -> ('a, Format.formatter, unit) format -> 'a
val everbose : int -> ('a, Format.formatter, unit) format -> 'a
val verbose  : int -> ('a, Format.formatter, unit) format -> 'a
val set_verbose : int -> unit

(* -------------------------------------------------------------- *)

type tool_opt = {
    pp_error  : bool;
    checkbool : bool;
  }

val time: ('a -> 'b) -> 'a -> float * 'b
