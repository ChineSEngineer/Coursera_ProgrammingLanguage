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

fun only_capitals (strs) =
    List.filter (fn str => Char.isUpper(String.sub(str, 0))) strs

fun longest_string1 (strs) =
    foldl (fn (x,y) => if String.size(x) > String.size(y) then x else y) "" strs

fun longest_string2 (strs) =
    foldl (fn (x,y) => if String.size(x) >= String.size(y) then x else y) "" strs
	  
fun longest_string_helper f strs =
    if f(2, 1)
    then longest_string1 strs
    else longest_string2 strs

val longest_string3 = longest_string_helper (fn(x,y) => x > y)
val longest_string4 = longest_string_helper (fn(x,y) => false)

fun longest_capitalized (strs) =
    longest_string1 (only_capitals strs)

fun rev_string (str) =
    String.implode (List.rev (String.explode str))
					    
fun first_answer f lst =
    case lst of
	[] => raise NoAnswer
       |x::xs => case f(x) of
		     NONE => first_answer f xs
		   | SOME i => i 

fun all_answers f lst =
    let
	fun helper lst acc =
	    case lst of
		[] => SOME acc
	      | x::xs => case f(x) of
			     NONE => NONE
			   | SOME i => helper xs (acc@[i])
    in
	helper lst []
    end

fun count_wildcards (p) =
    g (fn () => 1) (fn _ => 0) p

fun count_wild_and_variable_lengths (p) =
    g (fn () => 1) (fn i => String.size(i)) p

fun count_some_var (str, p) =
    g (fn ()=> 0) (fn i => if i = str then 1 else 0) p


fun check_pat (p) =
    let
	fun helper (p, names) =
	    case p of
		Variable str => names@[str]
	      | TupleP ps => foldl (fn (p, acc) => helper(p, acc)) names ps 
	      | ConstructorP (_, p)=> helper(p, names)
	      | _ => names

	fun is_distinct (lst, record) =
	    case lst of
		[] => true
	      | x::xs =>if List.exists (fn str:string => str = x) record
			then false
			else is_distinct(xs, record@[x])
    in
	is_distinct(helper(p, []), [])
    end

						   
fun match (va, pa) =
    let			 
	fun helper (va, pa, acc) = 
	    case (va, pa) of
		(Const l, ConstP r) => if l = r then SOME acc else NONE
	      | (Constructor (str1, v), ConstructorP (str2, p)) => if str1 = str2  then helper(v, p, acc) else NONE
	      | (Tuple vs, TupleP ps) =>
		(case ( all_answers (fn (v, p) => helper(v, p, [])) (ListPair.zip(vs, ps)) ) of
		    NONE => NONE
		  | SOME i => SOME (List.concat(i))) 
	      | (Unit, UnitP) => SOME []
	      | (_, Variable var) => SOME [(var, va)]
	      | (_, Wildcard) => SOME []
	      | _ => NONE
    in
	helper(va, pa, [])
    end

fun first_match va pas =
    SOME (first_answer (fn pa => match(va, pa)) pas)
    handle NoAnswer => NONE

