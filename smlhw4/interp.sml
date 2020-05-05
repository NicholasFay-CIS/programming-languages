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
                | RES_SUCC | RES_PRED | RES_ISZERO | RES_FUN of (string * term) ;

(*  An environment is a function string -> result  *)

exception UnboundID
exception Error of string

datatype env = Env of (string -> result)

fun emptyenvFun  (x : string) : result = raise UnboundID;
val emptyenv = Env emptyenvFun

(*  update : env -> string -> result -> string -> result  *)
fun update (Env e) (x : string) (ty : result) = fn y => if x = y then ty else e y

fun interp (env, AST_ID i)          = let val Env e = env in e i end 
  | interp (env, AST_NUM n)         = RES_NUM n
  | interp(env, AST_BOOL b)        = RES_BOOL b
  | interp (env, AST_FUN (i,e))     = RES_FUN (i,e)
  | interp (env, AST_APP (AST_APP (AST_ADD, e1), e2)) = 
                       (case interp (env, e1) of
                          RES_NUM n1 => (case interp (env, e2) of
                                          RES_NUM n2 => RES_NUM (n1+n2)
                                        |  _         => raise (Error  "not a number"))
                        |  _         => raise (Error "not a number")
                       )
  | interp (env, AST_APP (AST_ADD, e1)) = raise (Error "not enough arguments for +")
  | interp (env, AST_APP (e1,e2))   =  (case interp (env, e1) of
         RES_FUN (v, body) => let val v2 = interp (env,e2)
                               in let val new_env = update env v v2
                                   in interp (Env new_env, body) 
                                   end
                              end  

       | RES_SUCC => (case interp(env, e2) of 
                RES_NUM num => RES_NUM (num+1)
              | RES_ERROR err => RES_ERROR err)

       | RES_PRED => (case interp(env, e2) of 
                RES_NUM 0 => RES_NUM 0
              | RES_NUM num => RES_NUM (num-1)
              | RES_ERROR err => RES_ERROR err)

       | RES_ISZERO => (case (interp(env, e2)) of 
                RES_NUM 0 => RES_BOOL true
              | RES_NUM num => RES_BOOL false
              | RES_ERROR err => RES_ERROR err) 

       | _ => raise Error ("Invalid pattern to match"))

  | interp (env, AST_SUCC)          = RES_SUCC
  | interp (env, AST_PRED)          = RES_PRED
  | interp (env, AST_ISZERO)        = RES_ISZERO
  | interp (env, AST_IF (e1,e2,e3)) = (case interp (env,e1) of
                                              RES_BOOL true => interp (env,e2)
                                            | RES_BOOL false => interp (env,e3)
                                            | _              => raise (Error "if condition non-bool!") )

  | interp (env, AST_LET (x,e1,e2)) = interp(Env(update env x (interp(env, e1))), e2) 


  | interp (env, AST_ERROR s)       = raise (Error s)

fun interp_dynamic s = interp (emptyenv, parsestr s)
                     handle (Error z) => RES_ERROR z ;
val test1 =  "let x = 1 in \
              \ let f = fn z => x in \
              \ let x = 99 \
              \ in f x       \      
              \ end \
           \ end \
       \  end  " ;

val x = interp_dynamic(test1);
val y = interp_dynamic(test1);
