(* Dan Grossman, CSE341, HW3 Provided Code *)

exception NoAnswer

datatype pattern = WildcardP
                 | VariableP of string
                 | UnitP
                 | ConstantP of int
                 | ConstructorP of string * pattern
                 | TupleP of pattern list

datatype valu = Constant of int
              | Unit
              | Constructor of string * valu
              | Tuple of valu list

fun g f1 f2 p =
    let 
        val r = g f1 f2 
    in
        case p of
            WildcardP         => f1 ()
          | VariableP x       => f2 x
          | ConstructorP(_,p) => r p
          | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
          | _                 => 0
    end

(**** you can put all your code here ****)

(* Problem 1 Write a function only_lowercase that takes astring listand returns astring listthat has onlythe  strings  in  the  argument  that  start  with  an  lowercase  letter.  
 Assume  all  strings  have  at  least  1character.  UseList.filter,Char.isLower, andString.subto make a 1-2 line solution. *)
fun only_lowercase strings = 
  List.filter (fn string_ => Char.isLower(String.sub(string_, 0))) strings

(* Problem 2 Write a functionl ongest_string1 that takes a string list and returns the longest string in the list.  If the list is empty, return "".  
In the case of a tie, return the string closest to the beginning of the list. Usefoldl,String.size, and no recursion (other than the implementation offoldlis recursive). *)
fun longest_string1 string_l = 
  List.foldl (fn (string_1, string_2) => if (String.size(string_1) > String.size(string_2)) then string_1 else string_2) "" string_l;

(* Problem 3 Same as two except if there is a tie, choose the one from the back*)
fun longest_string2 string_l = 
  List.foldl (fn (string_1, string_2) => if (String.size(string_1) >= String.size(string_2)) then string_1 else string_2) "" string_l;

(* Problem 4 *)
fun longest_string_helper func strings_l = 
  List.foldl(fn (string_1, string_2) => if func(String.size string_1, String.size string_2) then string_1 else string_2) "" strings_l

val longest_string3 = longest_string_helper (fn (string_1, string_2) => if(string_1 > string_2) then true else false)
val longest_string4 = longest_string_helper (fn (string_1, string_2) => if(string_1 >= string_2) then true else false)

(* Problem 5 *)
val longest_lowercase = longest_string1 o only_lowercase

(* Problem 6 *)
val caps_no_X_string = String.implode o List.filter(fn string_ => if Char.compare(string_, String.sub("X",0)) = EQUAL then false else true) o List.map Char.toUpper o String.explode

(* Problem 7 *)
fun first_answer func list_ = case list_ of
  [] => raise NoAnswer
  | x :: list_ => case func(x) of
     SOME v => v
     | NONE => first_answer func(list_)

(* Problem 8 *)
fun all_answers func list_ = 
  let fun all_answers_helper (list_, a) = case list_ of
    [] => SOME a
    | x :: list_ => case func x of
                  NONE => NONE
                  | SOME x => all_answers_helper(list_, x @ a)
  in
    let val my_acc = []
    in  
      all_answers_helper(list_,my_acc)
    end
  end

(* Problem 9 *)
(* A: g takes 2 functions and a pattern. The first function is used if a wildcard is passed in, otherwise the other function is used if its a variable. Then does this for the rest of the pattern*)
(* B *)
val count_wildcards = g (fn pat => 1) (fn pat => 0)
(* C *)
val count_wild_and_variable_lengths = g (fn pat => 1) (fn pat => String.size pat)
(* D *)
fun count_a_var (string_,pat) = g (fn pat => 0) (fn s => if string_ = s then 1 else 0) pat

(* Problem 10 *)
fun check_pat pattern =
  let fun check_pat_helper_1 pattern = case pattern of
           ConstructorP(_, p) => check_pat_helper_1 p (* ask about how to check for p and unitP *)
         | TupleP vs => List.foldl (fn (v, s) => (check_pat_helper_1 v) @ s) [] vs
         | VariableP var => [var]
         | _ => []

    fun check_pat_helper_2 string_l = case string_l of
          [] => true
          | string_ :: string_l => if (List.exists (fn str => String.compare(str, string_) = EQUAL) string_l)
                                    then false
                                   else check_pat_helper_2 string_l
  in
    let val deconstruct_pat = check_pat_helper_1 pattern 
      in 
        check_pat_helper_2(deconstruct_pat)
      end
  end

(* Problem 11 *)
fun match (valu, pattern) = case (valu, pattern) of
    (_, VariableP s) => SOME [(s, valu)]
  | (_, WildcardP) => SOME []
  | (Unit, UnitP) => SOME []
  | (Constant c, ConstantP cp) => if c = cp then SOME [] else NONE
  | (Constructor (s, v), ConstructorP (s2, pat)) => if s2 = s then match (v, pat) else NONE
  | (Tuple tval, TupleP tpat) => if List.length(tval) = List.length(tpat) then all_answers (fn (v, v1) => match(v, v1)) (ListPair.zipEq(tval, tpat)) else NONE
  | _ => NONE

(* Problem 12 *)
fun first_match value pat_l = case pat_l of
  [] => NONE
  | x :: xs => if match(value, x) = NONE then first_match value xs else match(value, x)
