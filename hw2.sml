(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (s, s_list) =
  case s_list of
      [] => NONE		    
    | first::rest => if same_string(first, s)
		     then SOME rest
		     else case all_except_option(s, rest) of
			      NONE => NONE
			    | SOME xs => SOME (first::xs)

fun get_substitutions1 (s_list_list, s) =
  case s_list_list of
      [] => []
    | x::xs' => case all_except_option(s, x) of
		    NONE => get_substitutions1(xs', s)
		  | SOME lst => lst @ get_substitutions1(xs', s)

fun get_substitutions2 (s_list_list, s) =
  let fun accumulator (s_list_list, acc) =
	case s_list_list of
	    [] => acc
	  | x::xs' => case all_except_option(s, x) of
			  NONE => accumulator(xs', acc)
			| SOME lst => accumulator(xs', acc @ lst )
  in accumulator (s_list_list, [])
  end

fun similar_names (lists_similar_names, full_name) =
  let val {first=x,middle=y,last=z} = full_name
  in
      let fun name_pos(similar_names, full_name) =
	    case similar_names of
		[] => []
	      | name::xs' => {first=name,middle=y,last=z} ::
			       name_pos(xs', full_name)			     
      in
	  full_name :: name_pos(get_substitutions1(lists_similar_names, x),
				full_name)
      end
  end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (card) =
  case card of
      (Clubs, _) => Black
    | (Spades, _) => Black
    | (Diamonds, _) => Red
    | (Hearts, _) => Red

fun card_value (card) =
  case card of
      (_, Jack) => 10
    | (_, Queen) => 10
    | (_, King) => 10
    | (_, Ace) => 11
    | (_, Num x) => x

fun remove_card (cs, c, e) =
    case cs of
        [] => raise e
      | x::xs' => if x = c
                  then xs'
		  else x::remove_card(xs', c, e)

fun all_same_color (cs) =
    case cs of
        [] => true
      | _::[] => true
      | head::(neck::tail) => card_color(head) = card_color(neck) andalso
                              all_same_color(neck::tail)
			
fun sum_cards (cs) =
    let fun accumulator (cs, acc) =
            case cs of
                [] => acc
              | x::xs' => accumulator(xs', card_value(x) + acc)
    in
        accumulator(cs, 0)
end

fun score (cs, goal) =
  let 
      fun preliminary_score(cs, goal) =
	if sum_cards(cs) > goal
	then 3 * (sum_cards(cs) - goal)
	else goal - sum_cards(cs)
  in
      if all_same_color(cs)
      then preliminary_score(cs, goal) div 2
      else preliminary_score(cs, goal)
  end

      
