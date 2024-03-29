(* Coursera Programming Languages, Homework 3, Provided Code (answers below)*)

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

fun only_capitals xs  = List.filter (fn(x) => Char.isUpper(String.sub(x, 0))) xs

fun longest_string1 xs = List.foldl (fn(x,y) => if String.size(x) > String.size(y) then x  else y) "" xs

fun longest_string2 xs = List.foldl (fn(x,y) => if String.size(x) >=  String.size(y) then x else y) "" xs

fun longest_string_helper f xs  = List.foldl (fn(x,y) => if f(x,y) then x else y) "" xs

val longest_string3 = longest_string_helper (fn(x,y) => String.size(x) > String.size(y))

val longest_string4 = longest_string_helper (fn(x,y) => String.size(x) >= String.size(y))
			    
val longest_capitalized = longest_string1 o only_capitals

val rev_string =  implode o  rev o String.explode

fun first_answer f xs = case xs of
			    [] => raise NoAnswer
			  | x::xs' => case f(x) of
					  NONE => first_answer f xs'
				       |  SOME v => v
								     
fun all_answers f xs =
  let fun all_acc(f, xs, acc) =
	case xs of
	    [] => SOME acc
	  | x::xs' => case f x of
			  NONE => NONE
			| SOME x => all_acc(f, xs', x @ acc)
  in
      all_acc(f, xs, [])	  
  end
      
val count_wildcards = g (fn() => 1) (fn(x) => 0)

val count_wild_and_variable_lengths = g (fn() => 1) (fn(x) => String.size(x))

val count_some_var = (fn(s, p) => g (fn() => 0) (fn(x) => if x = s then 1 else 0) p)

fun check_pat (pattern) =
    let
        fun list_variables (pattern) =
	    case pattern of
	        Variable x => [x]
	      | TupleP ps => List.foldl (fn (x,xs) =>
					    xs @ list_variables(x)) [] ps
	      | ConstructorP(_,pattern) => list_variables(pattern)
	      | _ => []
			 
        fun has_repeats (string_list) =
            case string_list of
                [] => true
              | x::xs' => if List.exists (fn s => s = x) xs' then false else has_repeats(xs')
    in
        has_repeats(list_variables(pattern))
end

fun match (valu, pattern) =
  case (valu, pattern) of
      (_, Wildcard) => SOME []
    | (v, Variable s) => SOME [(s, v)]
    | (Unit, UnitP) => SOME []
    | (Const i, ConstP j) => if i = j then SOME [] else NONE
    | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
                               then all_answers match (ListPair.zip(vs, ps))
                               else NONE
    | (Constructor(s2, v), ConstructorP(s1, p)) => if s1 = s2
                                                   then match(v, p)
                                                   else NONE
    | (_, _) => NONE

fun first_match valu patterns_list =
  SOME (first_answer (fn x => match(valu, x)) patterns_list) handle NoAnswer => NONE
