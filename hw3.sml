(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern
        
datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals (sList)  =
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) sList
    
fun longest_string1 (sList) = 
    List.foldl (fn (s, s') => if String.size s > String.size s' then s else s') "" sList

fun longest_string2 (sList) =
    List.foldl (fn (s, s') => if String.size s < String.size s' then s' else s) "" sList
    
fun longest_string_helper f sList=
    List.foldl (fn (s, s') => if f (String.size s, String.size s') then s else s') "" sList

val longest_string3 = longest_string_helper (fn (x,y) => x > y)

val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = implode o rev o explode

fun first_answer f aList =
    case aList of
          [] => raise NoAnswer
        | x :: xs => let val fx = f x
                     in
                        (case fx of 
                              NONE => first_answer f xs
                            | SOME v => v)
                     end
                     
fun all_answers foo aList =
    let fun f (aList, acc) =
            case aList of
                  [] => SOME acc
                | x :: xs => (case foo x of
                                    NONE => NONE
                                  | SOME v => f (xs, acc @ v))
    in f (aList, [])
    end