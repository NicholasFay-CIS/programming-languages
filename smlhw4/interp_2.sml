(*  Concrete syntax 
e :: x | n | true | false | succ | pred | iszero | if e then e else e 
       | fn x => e | e e | (e) | let x = e in e 

Ambiguous
 - How do you parse e f g  (e f) g (x) or  e (f g )
 - How do you parse fn f = f 0   (fn f = f) 0 or fn f = (f 0) 

Abstract syntax tree :
datatype term = AST_ID of string | AST_NUM of int | AST_BOOL of bool
  | AST_SUCC | AST_PRED | AST_ISZERO | AST_ADD |  AST_IF of (term * term * term)
  | AST_FUN of (string * term) | AST_APP of (term * term) 
  | AST_LET of (string * term * term) 
  | AST_ERROR of string
*)

use "parser.sml";

datatype result = RES_ERROR of string | RES_NUM of int| RES_BOOL  of bool
                | RES_SUCC | RES_PRED | RES_ISZERO | RES_CLOSURE of (string * term * env) and env = Env of (string -> result) | RES_FUN of (string * term);
(*  An environment is a function string -> result  *)

exception UnboundID
exception Error of string

(*datatype env = Env of (string -> result) *)

fun emptyenvFun  (x : string) : result = raise UnboundID;
val emptyenv = Env emptyenvFun

(*  update : env -> string -> result -> string -> result  *)
fun update (Env e) (x : string) (ty : result) = fn y => if x = y then ty else e y

fun interp_static (env, AST_ID i)          = let val Env e = env in e i end 
  | interp_static (env, AST_NUM n)         = RES_NUM n
  | interp_static(env, AST_BOOL b)        = RES_BOOL b
  | interp_static (env, AST_FUN (i,e))     = RES_CLOSURE(i,e,env)
  | interp_static (env, AST_APP (AST_APP (AST_ADD, e1), e2)) = 
                       (case interp_static (env, e1) of
                          RES_NUM n1 => (case interp_static (env, e2) of
                                          RES_NUM n2 => RES_NUM (n1+n2)
                                        |  _         => raise (Error  "not a number"))
                        |  _         => raise (Error "not a number")
                       )
  | interp_static (env, AST_APP (AST_ADD, e1)) = raise (Error "not enough arguments for +")
  | interp_static (env, AST_APP (e1,e2))   =  (case interp_static (env, e1) of
        
        RES_CLOSURE(v, e, env1) => let val v1 = interp_static(env, e2)
                                    in let val new_env = update env1 v v1
                                      in interp_static(Env new_env, e)
                                      end
                                    end

       | RES_SUCC => (case interp_static(env, e2) of 
                RES_NUM num => RES_NUM (num + 1)
              | RES_ERROR err => RES_ERROR err)

       | RES_PRED => (case interp_static(env, e2) of 
                RES_NUM 0 => RES_NUM 0
              | RES_NUM num => RES_NUM (num - 1)
              | RES_ERROR err => RES_ERROR err)

       | RES_ISZERO => (case (interp_static(env, e2)) of 
                RES_NUM 0 => RES_BOOL true
              | RES_NUM num => RES_BOOL false
              | RES_ERROR err => RES_ERROR err) 

       | _ => raise Error ("Invalid pattern to match"))

  | interp_static (env, AST_SUCC)          = RES_SUCC
  | interp_static (env, AST_PRED)          = RES_PRED
  | interp_static (env, AST_ISZERO)        = RES_ISZERO
  | interp_static (env, AST_IF (e1,e2,e3)) = (case interp_static (env,e1) of
                                              RES_BOOL true => interp_static (env,e2)
                                            | RES_BOOL false => interp_static (env,e3)
                                            | _              => raise (Error "if condition non-bool!") )

  | interp_static (env, AST_LET (x,e1,e2)) = interp_static(Env(update env x (interp_static(env, e1))), e2) 


  | interp_static (env, AST_ERROR s)       = raise (Error s)

fun interp_stat s = interp_static (emptyenv, parsestr s)
                     handle (Error z) => RES_ERROR z ;

