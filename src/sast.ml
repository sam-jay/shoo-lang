open Ast

type styp =
    SVoid
  | SInt
  | SFloat
  | SBool
  | SString
  | SFunc of sfunc_typ
  | SStruct of sstruct_typ
  | SArray of styp
  | SABSTRACT
  | SAny

and sfunc_typ = {
  sparam_typs: styp list;
  sreturn_typ: styp;
  sbuiltin: bool;
}

and sstruct_typ = {
  sstruct_name: string;
  smembers: (styp * sexpr option) StringMap.t;
  sincomplete: bool;
  signore: bool;
}

and snewable =
    SNArray of styp * sexpr
  | SNStruct of styp

and sexpr = styp * sx
and sx =
    SIntLit of int
  | SFloatLit of string
  | SStrLit of string
  | SBoolLit of bool
  | SArrayLit of sexpr list
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SPop of sexpr * pop 
  | SAssign of sexpr * sexpr
  | SFCall of sexpr * sexpr list
  | SFExpr of sfexpr
  | SStructInit of styp * (string * sexpr) list
  | SArrayAccess of sexpr * sexpr
  | SDot of sexpr * string
  | SNew of snewable
  | SClosure of sclsr
  | SNoexpr
and sbind = styp * string

and sfexpr = {
  sname : string;
  srecursive : bool; 
  styp : styp;
  sparams: sbind list;
  sbody : sstmt list
}

and sclsr = {
  ind: int;
  free_vars: sbind list;
}

and sstmt =
    SExpr of sexpr
  | SVBlock of sstmt list
  | SVDecl of styp * string * sexpr option
  | SReturn of sexpr
  | SIf of sexpr * sstmt list * sstmt list
  | SForLoop of (sstmt option) * (sexpr option) * (sexpr option) * sstmt list
  | SStructDef of string * (styp * string * sexpr option) list

type sprogram = sstmt list

let string_of_sstmt = function
  | SExpr(_) -> "SExpr"
  | _ -> "Other"

(* PRETTY PRINTING based off of printer.ml *)

let rec fmt_styp = function
    SVoid -> "svoid"
  | SFunc(e) -> "sfunc(" ^ 
                (String.concat "," (List.map fmt_styp e.sparam_typs)) ^ "; " 
                ^ (fmt_styp e.sreturn_typ) ^ ")" 
  | SInt -> "sint"
  | SFloat -> "sfloat"
  | SBool -> "sbool"
  | SString -> "sstring"
  | SStruct(st) -> fmt_four "sstruct" st.sstruct_name 
                     (fmt_list (List.map (fun (k, _) -> k) (StringMap.bindings st.smembers))) 
                     (string_of_bool st.sincomplete)
                     (string_of_bool st.signore)
  | SArray(t) -> fmt_one "sarray" (fmt_styp t)
  | SABSTRACT -> "SABSTRACT"
  | SAny -> "SAny"

let fmt_sparams l =
  let fmt_p = function
      (t, n) -> String.concat "" ["("; fmt_styp t; ", "; n; ")"] in
  fmt_list (List.map fmt_p l)

let rec fmt_sexpr (_,s) =
  (match s with
     SIntLit(l) -> fmt_one "IntLit" (string_of_int l)
   | SFloatLit(l) -> fmt_one "FloatLit" l
   | SStrLit(l) -> fmt_one "StrLit"  l
   | SBoolLit(l) -> fmt_one "BoolLit" (string_of_bool l)
   | SId(s) -> fmt_one "Id" s
   | SBinop(e1, o, e2) -> (fmt_sexpr e1) ^ "\n    " ^ (fmt_op o) ^ "\n    " 
                          ^ (fmt_sexpr e2)
   | SUnop(uo, e) -> fmt_two "Unop" (fmt_uop uo) (fmt_sexpr e)
   | SPop(e, po) -> fmt_two "Pop" (fmt_sexpr e) (fmt_pop po)
   | SAssign(e1, e2) -> fmt_two "Assign" (fmt_sexpr e1) (fmt_sexpr e2)
   | SArrayAccess(s, e) -> fmt_two "ArrayAccess" (fmt_sexpr s) (fmt_sexpr e)
   | SDot(e, s) -> fmt_two "Dot" (fmt_sexpr e) s
   | SFCall(se, a) -> "SFCall(\n      " ^ ((fmt_sexpr se) ^ "\n") ^ ("      " 
                                                                     ^ fmt_list (List.map fmt_sexpr a) ^ "\n    )")
   (* below actually is parsed with {name = e.name; param = e.params;
    * typ = e.typ; body = e.body}. See test programs for examples. *)
   | SFExpr(s) -> fmt_three "FExpr" (fmt_sparams s.sparams)
                    (fmt_styp s.styp) (fmt_sstmt_list s.sbody)
   | SStructInit(_, l) -> fmt_one "StructInit" (fmt_sinit l)
   | SArrayLit(l) -> fmt_one "ArrayLit" (fmt_list (List.map fmt_sexpr l))
   | SNew(t) -> fmt_one "New" (fmt_sn t)
   | SClosure(clsr) -> fmt_two "Closure" (string_of_int clsr.ind) 
                         (fmt_list (List.map (fun (t, n) -> fmt_styp t ^ " " ^ n) clsr.free_vars))
   | SNoexpr -> ""
  )

and fmt_sn = function
    SNArray(t, s) -> fmt_two "NArray" (fmt_styp t) (fmt_sexpr s)
  | SNStruct(n) -> fmt_one "NStruct" (fmt_styp n)

and fmt_smembers l =
  let fmt_m = function
      (t, n, None) -> fmt_three "" (fmt_styp t) n "None"
    | (t, n, Some(e)) -> fmt_three "" (fmt_styp t) n (fmt_sexpr e) in
  fmt_list (List.map fmt_m l)

and fmt_sinit l =
  let fmt_i (n, e) = fmt_two "" n (fmt_sexpr e) in
  fmt_list (List.map fmt_i l)

and fmt_sstmt = function
    SExpr(se) -> fmt_sexpr se
  | SReturn(e) -> "Return " ^ (fmt_sexpr e)
  | SVDecl (t, n, l) -> (fmt_styp t) ^ " " ^ n ^ " = " ^ (match l with 
        None -> "" | Some(e) -> fmt_sexpr e)
  | SForLoop (init, e2, e3, s) -> 
    fmt_four "ForLoop" 
      (match init with None -> "" | Some(s) -> fmt_sstmt s)
      (fmt_opt_sexpr e2) 
      (fmt_opt_sexpr e3) (fmt_sstmt_list s)
  | SStructDef(n, m) -> fmt_two "StructDef" n (fmt_smembers m)
  | SIf(e, tL, fL) -> fmt_three "If" (fmt_sexpr e) (fmt_sstmt_list tL) 
                        (fmt_sstmt_list fL)
  | SVBlock(_) -> "SVBlock"

and fmt_sstmt_list ?spacer l =
  let sstmts = List.map fmt_sstmt l in
  let s = match spacer with Some(s) -> s | _ -> "" in
  let sstmts = List.map (fun x -> s ^ x) sstmts in
  String.concat "\n" sstmts

and fmt_opt_sexpr = function
    None -> ""
  | Some(e) -> fmt_sexpr e

let string_of_sprogram sast =
  String.concat ";\n" (List.map fmt_sstmt sast)
