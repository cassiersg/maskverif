open Util

type ty = W1 | W8 | W16 | W32 | W64

let w1  = W1 
let w8  = W8
let w16 = W16
let w32 = W32
let w64 = W64

let ty_size = function
  | W1  -> 1
  | W8  -> 8
  | W16 -> 16
  | W32 -> 32
  | W64 -> 64

let ty2string ty = "w" ^ string_of_int (ty_size ty)
  
(* ----------------------------------------------------------------------- *)
type var = {
  v_id   : int;
  v_name : string;
  v_ty   : ty;
}

module V = struct

  let id = ref 0

  let mk_var s ty =
    let p = { v_id = !id; v_name = s; v_ty = ty } in
    incr id;
    p

  let clone v = mk_var v.v_name v.v_ty

  type t = var

  let equal v1 v2 = v1 == v2

  let hash v = v.v_id

  let compare v1 v2 = v1.v_id - v2.v_id

end

module Sv = Set.Make(V)
module Mv = Map.Make(V)
module Hv = Hashtbl.Make(V)

(* ----------------------------------------------------------------------- *)

type rnd = var
type param = var

let pp_var fmt v = Format.fprintf fmt "%s" v.v_name

let pp_rnd fmt r = Format.fprintf fmt "$%a" pp_var r

let pp_share fmt (p,s,v) =
  Format.fprintf fmt "%a.[%i]/%a/" pp_var p s pp_var v

(* ----------------------------------------------------------------------- *)
type op_kind = Add | Mul | Neg | Other

type operator = {
  op_id    : int;
  op_name  : string;
  op_ty    : (ty list * ty) option;
  op_bij   : bool; (* true means  bijective *)
  op_kind  : op_kind
}

module Op = struct
  let id = ref 0

  let tbl = Hashtbl.create 100
          
  let make s ty bij kind =
    if Hashtbl.mem tbl s then error "" None "duplicate operator %s" s;
    incr id;
    let o = {
        op_id = !id;
        op_name = s;
        op_ty = ty;
        op_bij = bij;
        op_kind = kind;
      } in
    Hashtbl.add tbl s o; o

  let find s = Hashtbl.find tbl s

  let equal o1 o2 = o1 == o2
  let hash o = o.op_id
  
end


(* ----------------------------------------------------------------------- *)

let o_mul2 = Op.make "mul2" (Some ([w1], w1)) true Other
let o_mul3 = Op.make "mul3" (Some ([w1], w1)) true Other
let o_square = Op.make "square" (Some ([w1], w1)) true Other

let mk_add t s = Op.make s (Some ([t;t], t)) true  Add
let mk_mul t s = Op.make s (Some ([t;t], t)) false Mul
let mk_neg t s = Op.make s (Some ([t],t))    true  Neg 


let o_addb   = mk_add w1  "^"
let o_addw8  = mk_add w8  "^w8"
let o_addw16 = mk_add w16 "^w16"
let o_addw32 = mk_add w32 "^w32"
let o_addw64 = mk_add w64 "^w64"

let o_mulb   = mk_mul w1  "&"
let o_mulw8  = mk_mul w8  "&w8"
let o_mulw16 = mk_mul w16 "&w16"
let o_mulw32 = mk_mul w32 "&w32"
let o_mulw64 = mk_mul w64 "&w64"

let o_negb   = mk_neg w1  "~"
let o_negw8  = mk_neg w8  "~w8"
let o_negw16 = mk_neg w16 "~w16"
let o_negw32 = mk_neg w32 "~w32"
let o_negw64 = mk_neg w64 "~w64"

let o_neg = function
  | W1  -> o_negb
  | W8  -> o_negw8
  | W16 -> o_negw16
  | W32 -> o_negw32
  | W64 -> o_negw64

let o_add = function
  | W1  -> o_addb
  | W8  -> o_addw8
  | W16 -> o_addw16
  | W32 -> o_addw32
  | W64 -> o_addw64

let o_mul = function
  | W1  -> o_mulb
  | W8  -> o_mulw8
  | W16 -> o_mulw16
  | W32 -> o_mulw32
  | W64 -> o_mulw64

let o_tuple = Op.make "" None false Other

let _DFF_P_     = Op.make "$_DFF_P_" (Some([w1;w1],w1)) false Other
let _DFF_PP0_   = Op.make "$_DFF_PP0_" (Some([w1;w1;w1],w1)) false Other
let _DFFSR_PPP_ = Op.make "$_DFFSR_PPP_" (Some([w1;w1;w1],w1)) false Other
let _DFF_PN0_   = Op.make "$_DFF_PN0_" (Some([w1;w1;w1;w1],w1)) false Other

let is_FF_op o =
  Op.equal o _DFF_PP0_ || Op.equal o _DFFSR_PPP_ || Op.equal o _DFF_PN0_

(* ----------------------------------------------------------------------- *)

type constant = {
    c_ty : ty;
    c_val : Z.t;
  }

module C = struct 
  type t = constant

  let equal c1 c2 = 
    c1.c_ty == c2 .c_ty && Z.equal c1.c_val c2.c_val 

  let hash c = Z.hash c.c_val 

  let pp fmt c = 
    if c.c_ty == W1 then 
      Format.fprintf fmt "%s" (Z.format "%b" c.c_val)
    else
      Format.fprintf fmt "%s" (Z.format "%X" c.c_val)

  let zero ty =
    { c_ty = ty; c_val = Z.zero }

  let one ty = 
    { c_ty = ty; c_val = Z.pred (Z.shift_left Z.one (ty_size ty)) }

  let make ty z = 
    assert (Z.leq Z.zero z && Z.leq z (one ty).c_val);
    { c_ty = ty; c_val = z }
     
  let _true = one w1 
  let _false = zero w1

end

type expr = {
  e_id : int;
  e_node : expr_node}

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

let e_equal e1 e2 = e1 == e2
let e_hash e = e.e_id

module HE = struct

  type t = expr

  let equal_node n1 n2 =
    match n1, n2 with
    | Etop         , Etop          -> true
    | Ernd r1      , Ernd r2       -> V.equal r1 r2
    | Eshare(p1,i1,v1), Eshare(p2,i2,v2) ->
      i1 == i2 && V.equal p1 p2 && V.equal v1 v2
    | Epub x1      , Epub x2       -> V.equal x1 x2
    | Eop1(o1,e1)  , Eop1(o2,e2)   -> Op.equal o1 o2 && e_equal e1 e2
    | Eop2(o1,e11,e12), Eop2(o2,e21,e22) -> 
      Op.equal o1 o2 && e_equal e11 e21 && e_equal e12 e22
    | Eop(b1,o1,es1), Eop(b2,o2,es2) ->
      b1 = b2 && Op.equal o1 o2 && Array.for_all2 e_equal es1 es2
    | Econst c1, Econst c2 -> C.equal c1 c2 
    | _                    -> false

  let equal e1 e2 = equal_node e1.e_node e2.e_node

  let combine h tag = h lsl 3 + tag
  let combine2 h1 h2 tag = combine (combine h1 h2) tag
  let combine3 h1 h2 h3 tag = combine (combine (combine h1 h2) h3) tag

  let hash_node = function
    | Etop          -> 0
    | Ernd r        -> combine (V.hash r) 1
    | Eshare(p,i,v) -> combine2 (combine (V.hash v) (V.hash p)) i 2
    | Epub x        -> combine (V.hash x) 3
    | Eop1(o1,e1)   -> combine2 (Op.hash o1) (e_hash e1) 4
    | Eop2(o,e1,e2) -> combine3 (Op.hash o) (e_hash e1) (e_hash e2) 5
    | Eop(_b,o,es)  ->
      let h =
        Array.fold_left (fun h e -> combine h (e_hash e)) (Op.hash o) es in
      combine h 7
    | Econst(c)     -> C.hash c

  let hash e = hash_node e.e_node

end

module E = struct

  module W = Weak.Make(HE)

  let id = ref 0
  let h = W.create 5000

  let mk_expr e =
    let e = { e_id = -1; e_node = e } in
    try W.find h e
    with Not_found ->
      let e = { e with e_id = !id } in
      W.add h e; incr id;
      e

  type t = expr

  let equal e1 e2 = e1 == e2

  let hash e = e.e_id

  let compare e1 e2 = e1.e_id - e2.e_id

end

module He = Hashtbl.Make(E)

module Se = Set.Make(E)

module Me = Map.Make(E)

let top       = E.mk_expr Etop
let rnd r     = E.mk_expr (Ernd r)
let pub x     = E.mk_expr (Epub x)
let share p i v =
    (*
    Format.printf "### share p=%a i=%i v=%a\n" pp_var p i pp_var v;
    *)
    E.mk_expr (Eshare(p,i,v))

let econst c = E.mk_expr (Econst c)
let ezero ty = econst (C.make ty Z.zero)
let eone ty = econst (C.make ty Z.one)
   
let etrue = econst (C._true)
let efalse = econst (C._false)

let op1 o e = E.mk_expr (Eop1(o,e))
let op2 o e1 e2 = E.mk_expr(Eop2(o,e1,e2))

let eneg ty e = op1 (o_neg ty) e
let eadd ty e1 e2 = op2 (o_add ty) e1 e2
let emul ty e1 e2 = op2 (o_mul ty) e1 e2

let neg e = op1 o_negb e
let add e1 e2 = op2 o_addb e1 e2
let mul e1 e2 = op2 o_mulb e1 e2

let mul2 e = op1 o_mul2 e
let mul3 e = op1 o_mul3 e
let square e = op1 o_square e

let unsafe_op b o es = E.mk_expr (Eop(b,o,es))

let op_nodup o es = E.mk_expr (Eop(true,o, es))

let op o es  =
  let n = Array.length es in
  let tbl = He.create (2 * n) in
  let rec check i =
    n <= i ||
      (not (He.mem tbl es.(i)) &&
         (He.add tbl es.(i) ();check (i+1))) in
  let b = check 0 in
  unsafe_op b o es

let is_op_tuple o = Op.equal o o_tuple

let tuple_nodup es = op_nodup o_tuple es

let tuple es = op o_tuple es

let is_rnd e =
  match e.e_node with
  | Ernd _ -> true
  | _      -> false

(* --------------------------------------------------------------------- *)
type top = [ `Top |  `AddL | `AddR | `MulL | `MulR | `Not]

let pp_op1 aux fmt o e = 
  if o.op_kind = Neg then
    Format.fprintf fmt "%s %a" o.op_name (aux `Not) e
  else 
    Format.fprintf fmt "%s(%a)" o.op_name (aux `Top) e

let pp_op2 aux fmt top o e1 e2 = 
   match o.op_kind with
   | Add ->
     begin match top with
     | `AddR | `MulL | `MulR | `Not ->
       Format.fprintf fmt "(%a %s %a)" 
         (aux `AddL) e1 o.op_name (aux `AddR) e2
     | _ ->
       Format.fprintf fmt "%a %s %a" 
         (aux `AddL) e1 o.op_name (aux `AddR) e2
     end
   | Mul ->
     begin match top with
     | `MulR | `Not ->
       Format.fprintf fmt "(%a %s %a)" 
         (aux `MulL) e1 o.op_name (aux `MulR) e2
     | _ ->
       Format.fprintf fmt "%a %s %a" 
         (aux `MulL) e1 o.op_name (aux `MulR) e2
     end
   | _   ->
     Format.fprintf fmt "%s(@[<hov 2>%a,@ %a@])" o.op_name 
       (aux `Top) e1 (aux `Top) e2

let pp_op aux fmt o es = 
  if Array.length es <> 0 then
    Format.fprintf fmt "%s(@[<hov 2>%a@])" o.op_name
      (pp_list ",@ " (aux `Top)) (Array.to_list es)
  else
    Format.fprintf fmt "%s" o.op_name

let pp_expr fmt e =
  let rec aux top fmt e =
    match e.e_node with
    | Etop            -> Format.fprintf fmt "TOP"
    | Ernd r          -> pp_rnd fmt r
    | Eshare(p,i,v)   -> pp_share fmt (p,i,v)
    | Epub x          -> pp_var fmt x
    | Econst c        -> C.pp fmt c 
    | Eop1(o, e)      -> pp_op1 aux fmt o e
    | Eop2(o, e1, e2) -> pp_op2 aux fmt top o e1 e2
    | Eop(_, o, es)   -> pp_op aux fmt o es in
  aux `Top fmt e

(* ---------------------------------------------------------------------- *)
let type_of_expr e = 
  match e.e_node with 
  | Etop -> None
  | Ernd v | Eshare (_,_,v) | Epub v -> Some v.v_ty
  | Econst c -> Some c.c_ty
  | Eop1(op,_) | Eop2(op,_,_) | Eop(_,op,_) -> 
    match op.op_ty with
    | Some(_,codom) -> Some(codom)
    | None -> None
