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
    
val count_wildcards = g (fn x => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn x => 1) (fn s => String.size s)

fun count_some_var (s, p) = g (fn x => 0) (fn x => if s = x then 1 else 0) p

fun check_pat p =
    let
        fun getVars p = 
            case p of
                  Variable s => [s]
                | TupleP ps => List.foldl (fn (p, p') => (getVars p) @ p') [] ps
                | ConstructorP(_,p) => getVars p
                | _ => []
        
        fun duplicateElExist lst =
            case lst of
                  [] => false
                | x :: xs => (List.exists (fn y => x = y) xs) orelse duplicateElExist xs
    in
        not (duplicateElExist (getVars p))
    end

fun match (v, p) =
    case (v, p) of
          (_, Wildcard) => SOME []
        | (Unit, UnitP) => SOME []
        | (Const n, ConstP n') => if n = n' then SOME [] else NONE
        | (_, Variable s) => SOME [(s, v)]
        | (Constructor (s1, v'), ConstructorP (s2, p')) => if s1 = s2
                                                           then match (v', p')
                                                           else NONE
        | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
                                   then all_answers match (ListPair.zip (vs, ps))
                                   else NONE
        | _ => NONE

fun first_match v ps =
    SOME (first_answer (fn p => match (v, p)) ps)
    handle NoAnswer => NONE