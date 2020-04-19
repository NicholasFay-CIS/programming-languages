(* CSE 341, HW2 Provided Code *)

(* main datatype definition we will use throughout the assignment *)
datatype json =
         Num of real (* real is what SML calls floating point numbers *)
       | String of string
       | False
       | True
       | Null
       | Array of json list
       | Object of (string * json) list

(* some examples of values of type json *)
val json_pi    = Num 3.14159
val json_hello = String "hello"
val json_false = False
val json_array = Array [Num 1.0, String "world", Null]
val json_obj   = Object [("foo", json_pi), ("bar", json_array), ("ok", True)]

(* some provided one-liners that use the standard library and/or some features
   we have not learned yet. (Only) the challenge problem will need more
   standard-library functions. *)

(* dedup : string list -> string list -- it removes duplicates *)
fun dedup xs = ListMergeSort.uniqueSort String.compare xs

(* strcmp : string * string -> order compares strings alphabetically
   where datatype order = LESS | EQUAL | GREATER *)
val strcmp = String.compare                                        
                        
(* convert an int to a real *)
val int_to_real = Real.fromInt

(* absolute value of a real *)
val real_abs = Real.abs

(* convert a real to a string *)
val real_to_string = Real.toString

(* return true if a real is negative : real -> bool *)
val real_is_negative = Real.signBit

(* We now load 3 files with police data represented as values of type json.
   Each file binds one variable: small_incident_reports (10 reports), 
   medium_incident_reports (100 reports), and large_incident_reports 
   (1000 reports) respectively.

   However, the large file is commented out for now because it will take 
   about 15 seconds to load, which is too long while you are debugging
   earlier problems.  In string format, we have ~10000 records -- if you
   do the challenge problem, you will be able to read in all 10000 quickly --
   it's the "trick" of giving you large SML values that is slow.
*)

(* Make SML print a little less while we load a bunch of data. *)
       ; (* this semicolon is important -- it ends the previous binding *)
Control.Print.printDepth := 3;
Control.Print.printLength := 3;

use "parsed_small_police.sml";
use "parsed_medium_police.sml";

(* uncomment when you are ready to do the problems needing the large report*)
(* use "parsed_large_police.sml"; *)

val large_incident_reports_list =
    case small_incident_reports of
        Array js => js
      | _ => raise (Fail "expected large_incident_reports to be an array")


(* Now make SML print more again so that we can see what we're working with. *)
; Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 1-8 HERE ****)

(* Problem 1 *)
fun make_silly_json i =
  let fun silly_json_helper i = 
    if i = 0 
      then []
    else 
      (Object [("n", Num (int_to_real i)), ("b", True)]) :: silly_json_helper (i-1)
  in
    Array (silly_json_helper i)
  end 

(* Problem 2 *)
fun assoc (k, xs) = case xs of
  [] => NONE
  | (k1, v1) :: xs => if k = k1 then SOME v1 else assoc (k, xs)

(* Problem 3 *)
fun dot(j, f) = case j of
  Object fs => assoc(f, fs)
  | _ => NONE

(* Problem 4 *)
fun one_fields(json) = case json of
  Object xs => let
    fun one_fields_helper(xs, a) = case xs of
      [] => a | (x,_) :: xs => one_fields_helper(xs, x::a)
    in 
      one_fields_helper(xs, [])
    end
  | _ => []

(* Problem 5 *)
fun no_repeats string_l = 
  length(string_l) = length(dedup string_l)

(* Problem 6 *)
fun recursive_no_field_repeats json =
  let 
    fun r_n_r_helper_1 xs = case xs of
      [] => true 
      | x :: xs => recursive_no_field_repeats x andalso r_n_r_helper_1 xs

    fun r_n_r_helper_2 xs = case xs of 
      [] => true 
      | (_,json) :: xs => recursive_no_field_repeats json andalso r_n_r_helper_2 xs

  in case json of
    Array xs => r_n_r_helper_1 xs 
    | Object xs => no_repeats(one_fields (Object xs)) andalso r_n_r_helper_2 xs 
    | _ => true

  end

(* Probelem 7 *)
fun count_occurrences (string_l, ex) =
  let
    fun co_helper (string_l, check, char_, count_, a) = case string_l of
        [] => (char_, count_) :: a
        | ch :: string_l => if ch = char_ then co_helper(string_l, check, char_, count_ + 1, a) 
                            else if check = EQUAL orelse check = strcmp(ch, char_) then co_helper(string_l, strcmp(ch, char_), ch, 1, (char_, count_) :: a)
                         else raise ex
  in
    case string_l of 
      [] => []
      | ch :: string_l => co_helper(string_l, EQUAL, ch, 1,[])
  end

(* Problem 8 *)
fun string_values_for_field(string_l, json_list) = case json_list of 
  [] => []
  | json :: json_list => case dot(json, string_l) of SOME (String x) => x :: string_values_for_field(string_l, json_list)
  | _ => string_values_for_field(string_l, json_list)  

(* histogram and historgram_for_field are provided, but they use your 
   count_occurrences and string_values_for_field, so uncomment them 
   after doing earlier problems *)

(* histogram_for_field takes a field name f and a list of objects js and 
   returns counts for how often a string is the contents of f in js. *)

exception SortIsBroken

fun histogram (xs : string list) : (string * int) list =
  let
    fun compare_strings (s1 : string, s2 : string) : bool = s1 > s2

    val sorted_xs = ListMergeSort.sort compare_strings xs
    val counts = count_occurrences (sorted_xs,SortIsBroken)

    fun compare_counts ((s1 : string, n1 : int), (s2 : string, n2 : int)) : bool =
      n1 < n2 orelse (n1 = n2 andalso s1 < s2)
  in
    ListMergeSort.sort compare_counts counts
  end

fun histogram_for_field (f,js) =
  histogram (string_values_for_field (f, js))

(**** PUT PROBLEMS 9-11 HERE ****)

(* Problem 9 *)
fun filter_field_value (string_1, string_2, json_l) = case json_l of
    [] => []
    | json :: json_l => case dot (json, string_1) of
                SOME (String v1) => if v1 = string_2 then json :: filter_field_value(string_1, string_2, json_l) else filter_field_value(string_1, string_2, json_l)
    | _ => filter_field_value(string_1, string_2, json_l)

(* Problem 10 *)
val large_event_clearance_description_histogram = histogram_for_field("event_clearance_description", large_incident_reports_list)

(* Problem 11 *)
val large_hundred_block_location_histogram = histogram_for_field("hundred_block_location", large_incident_reports_list)

;Control.Print.printDepth := 3;
Control.Print.printLength := 3;

(**** PUT PROBLEMS 12-15 HERE ****)

(* Problem 12 *)
val forty_third_and_the_ave_reports = filter_field_value("hundred_block_location", "43XX BLOCK OF UNIVERSITY WAY NE", large_incident_reports_list)

(* Problem 13 *)
val forty_third_and_the_ave_event_clearance_description_histogram = histogram_for_field("event_clearance_description", forty_third_and_the_ave_reports)

(* Problem 14 *)
val nineteenth_and_forty_fifth_reports = filter_field_value("hundred_block_location", "45XX BLOCK OF 19TH AVE NE", large_incident_reports_list)

(* Problem 15 *)
val nineteenth_and_forty_fifth_event_clearance_description_histogram = histogram_for_field("event_clearance_description", nineteenth_and_forty_fifth_reports)

;Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 16-19 HERE ****)

(* Problem 16 *)
fun concat_with (sep_string, string_l) = case string_l of
    [] => ""
    | [string_h] => string_h
    | (string_1 :: string_l) => string_1 ^ sep_string ^ concat_with(sep_string, string_l) 

(* Problem 17 *)
fun quote_string string_ = 
  "\"" ^ string_ ^ "\""

(* Problem 18 *)
fun real_to_string_for_json real_ = 
  (if real_is_negative real_ then "-" else "") ^ real_to_string(real_abs real_)
   
(* Problem 19 *)
fun json_to_string json = 
  let
    fun array_helper xs = case xs of
        [] => []
        | x :: xs => json_to_string x :: array_helper xs

    fun obj_helper str_json_l = case str_json_l of
        [] => []
        | (str, j_o) :: str_json_l => ((quote_string str) ^ " : " ^ (json_to_string j_o)) :: obj_helper str_json_l
  in 
    case json of 
      Num real_ => real_to_string_for_json real_
      | String str => quote_string str
      | False => "false"
      | True => "true"
      | Null => "null"
      | Array json_l => "[" ^ concat_with(", ", array_helper json_l) ^ "]"
      | Object object => "{" ^ concat_with(", ", obj_helper object) ^ "}"
  end

(* For CHALLENGE PROBLEMS, see hw2challenge.sml *)

