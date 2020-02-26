(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_1 = only_capitals ["Adam", "brian", "Cindy"] = ["Adam","Cindy"]

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2_1 = longest_string1 ["abc", "", "", "eeeee"] = "eeeee"
val test2_2 = longest_string1 ["abc","bc","cab"] = "abc"
val test2_3 = longest_string1 [""] = ""

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3_1 = longest_string2 ["A","B","C"] = "C"
val test3_2 = longest_string2 [""] = ""
val test3_3 = longest_string2 ["abc", "bc", "cab"] = "cab"

val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4a_1 = longest_string3 ["abc", "", "", "eeeee"] = "eeeee"
val test4a_2 = longest_string3 ["abc","bc","cab"] = "abc"
val test4a_3 = longest_string3 [""] = ""

val test4b = longest_string4 ["A","B","C"] = "C"
val test4b_1 = longest_string4 ["A","bc","C"] = "bc"
val test4b_2 = longest_string4 ["A","B","C"] = "C"
val test4b_3 = longest_string4 [""] = ""
val test4b_4 = longest_string4 ["abc", "bc", "cab"] = "cab"


val test5 = longest_capitalized ["A","bc","C"] = "A"
val test5_1 = longest_capitalized ["ab","abc","abba"] = ""


val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7_1 = ((first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,0];false) handle NoAnswer => true)
val test7_2 = ((first_answer (fn x => if x > 3 then SOME x else NONE) []; false) handle NoAnswer => true)

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_1 = all_answers (fn x => if x > 3 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_2 = all_answers (fn x => if x > 3 then SOME [x] else NONE) [4,5,6,7] = SOME [4,5,6,7]

val test9a = count_wildcards Wildcard = 1
val test9a_1 = count_wildcards (TupleP [Variable "a", Wildcard, Variable "c0", Wildcard, ConstructorP ("c1", Wildcard)]) = 3

val test9b = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "a", ConstructorP ("c1", Wildcard), ConstructorP ("c0", Variable "iam14charslong")]) = 17

val test9c = count_some_var ("x", Variable("x")) = 1
val test9c_1 = count_some_var ("c0", (TupleP [Variable "a", Wildcard, Variable "c0", Wildcard, ConstructorP ("c0", Variable "c0")])) = 2
(*
val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []

*)