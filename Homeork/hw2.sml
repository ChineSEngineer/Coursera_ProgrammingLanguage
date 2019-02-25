(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same string), then you avoid several of the functions in problem 1 having polymorphic types that may be confusing *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str, string_list) =
    let
	fun helper(lst, saved_list) =
	    case lst of
		[] => NONE
	      | x::xs => if same_string(str, x)
			 then SOME (saved_list@xs)
			 else helper(xs, saved_list@[x])
    in
	helper(string_list, [])
    end

fun get_substitutions1 (sub, str) =
    case sub of
	[] => []
      | x::xs =>
	case all_except_option(str, x) of
	    NONE => get_substitutions1(xs, str)
	  | SOME i => i@get_substitutions1(xs, str)

fun get_substitutions2(sub, str) =
    let
	fun helper(sub_list , res) =
	    case sub_list of
		[] => res
	      | x::xs => case all_except_option(str, x) of
			     NONE => helper(xs, res)
			   | SOME i => helper(xs, res@i) 
    in
	helper(sub, [])
    end

fun similar_names(sub, {first=x, middle=y, last=z}) =
    let
	fun helper(lst) =
	    case lst of
		[] => []
	      | head::tail => [{first=head, middle=y, last=z}] @ helper(tail)
								    
    in
	 {first=x, middle=y, last=z}::helper(get_substitutions2(sub, x))
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

fun card_color (s, r) =
    case s of
	Clubs => Black
      | Diamonds => Red
      | Hearts => Red
      | Spades => Black

fun card_value (s, r) =
    case r of
	Num i => i
      | Ace => 11
      | _ => 10

fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | x::xs => if x = c
		 then xs
		 else x::remove_card(xs, c, e)

fun all_same_color (cs) =
    case cs of
	[] => true
      | x::[] => true
      | x::y::xs => if card_color(x) = card_color(y)
		    then all_same_color(y::xs)
		    else false

fun sum_cards(cs) =
    let
	fun helper(cs, cur) =
	    case cs of
		[] => cur
	      | x::xs => helper(xs, cur + card_value(x))
    in
	helper(cs, 0)
    end

fun score(cs, goal) =
    let
	val sum = sum_cards(cs)
	val same = all_same_color(cs)
    in
	if sum > goal
	then
	    if same
	    then (sum - goal) * 3 div 2
	    else (sum - goal) * 3
	else
	    if same
	    then (goal - sum) div 2
	    else goal - sum
    end

fun officiate(deck, mv, goal) =
    let
	fun helper ([], Draw::mv_tl, hold) = score(hold, goal)
	  | helper (deck, [], hold) = score(hold,goal)					  
	  | helper (deck_hd::deck_tl, Draw::mv_tl ,hold) = if sum_cards(hold@[deck_hd]) > goal
							   then score(hold@[deck_hd], goal) 
							   else helper(deck_tl, mv_tl, hold@[deck_hd])
	  | helper (deck, (Discard i)::mv_tl, hold) = helper(deck, mv_tl, remove_card(hold, i, IllegalMove))
    in
	helper(deck, mv, [])
    end


fun careful_player(deck, goal) =
    let
	fun check_remove(hold, cur, next_card) =
	    case hold of
		[] => (Clubs, Num 0)
	      | x::xs => if cur - card_value(x) + card_value(next_card) = 0
			 then x
			 else check_remove(xs, cur, next_card)
					  
	fun helper([], hold, move) = move
	  | helper (deck_hd::deck_tl, hold, move) =
	    let
		val prev =  sum_cards(hold)
		val next = prev + card_value(deck_hd)
	    in
		if next = goal
		then move
		else if prev < goal - 10
		then helper(deck_tl, hold@[deck_hd], move@[Draw])
		else
		    let
			val tmp = check_remove(hold, prev, deck_hd)
		    in
			if card_value(tmp) <> 0
			then helper(deck_hd::deck_tl, remove_card(hold, tmp, IllegalMove), move@[Discard tmp])
			else move
		    end		 
	    end
    in
	helper(deck, [], [])
    end








































	
fun score_challenge(cs, goal) =
    let
	val same = all_same_color(cs)
	fun sum_to_score(sum) =
	    if sum > goal
	    then
		if same
		then (sum - goal) * 3 div 2
		else (sum - goal) * 3
	    else
		if same
		then (goal - sum) div 2
		else goal - sum
				
	fun helper(cs, sum) =
	    case cs of
		[] => sum_to_score(sum)
	      | (st, rk)::xs => if rk <> Ace
			 then helper(xs, sum + card_value(st, rk))
			 else
			     let
				 val a = helper(xs, sum + 1)
				 val b = helper(xs, sum + 10)
			     in
				 if a < b
				 then a
				 else b
			     end
		  
    in
	helper(cs, 0)
    end

fun officiate_challenge(deck, mv, goal) =
    let
	fun sum_cards_min(cs) =
	    let
		fun helper(cs, cur) =
		    case cs of
			[] => cur
		      | x::xs => case x of
				     (_, Ace) => helper(xs, cur + 1)
				    |_ => helper(xs, cur + card_value(x))
	    in
		helper(cs, 0)
	    end
		
	fun helper ([], Draw::mv_tl, hold) = score_challenge(hold, goal)
	  | helper (deck, [], hold) = score_challenge(hold,goal)					  
	  | helper (deck_hd::deck_tl, Draw::mv_tl ,hold) = if sum_cards_min(hold@[deck_hd]) > goal
							   then score(hold@[deck_hd], goal) 
							   else helper(deck_tl, mv_tl, hold@[deck_hd])
	  | helper (deck, (Discard i)::mv_tl, hold) = helper(deck, mv_tl, remove_card(hold, i, IllegalMove))
    in
	helper(deck, mv, [])
    end
	
(*
fun score_challenge(cs, goal) =
    let
	fun card_value_challenge (s, r) =
	    case r of
		Num i => i
	      | Ace => 1
	      | _ => 10

	fun sum_cards_challenge (cs) =
	    let
		fun helper(cs, cur) =
		    case cs of
			[] => cur
		      | x::xs => helper(xs, cur + card_value_challenge(x))
	    in
		helper(cs, 0)
	    end
				
	val min_sum = sum_cards_challenge(cs)
	val max_sum = sum_cards(cs)
	val same = all_same_color(cs)

	fun helper(sum) =
	    if sum > goal
	    then
		if same
		then (sum - goal) * 3 div 2
		else (sum - goal) * 3
	    else
		if same
		then (goal - sum) div 2
		else goal - sum
				 
	fun helper_score (sum, NONE) = helper_score(sum + 10, SOME(helper(sum)))
	  | helper_score (sum, SOME res) =
	    if sum > max_sum
	    then res
	    else
		let
		    val tmp = helper(sum)
		in
		    if tmp < res
		    then
			helper_score(sum + 10, SOME(tmp))
		    else
			helper_score(sum + 10, SOME(res))
		end
    in
	helper_score(min_sum, NONE)
    end

fun officiate_challenge (deck, mv, goal) =
    let
	fun helper ([], Draw::mv_tl, hold) = score_challenge(hold, goal)
	  | helper (deck_hd::deck_tl, Draw::mv_tl ,hold) = helper(deck_tl, mv_tl, hold@[deck_hd])
	  | helper (deck, (Discard i)::mv_tl, hold) = helper(deck, mv_tl, remove_card(hold, i, IllegalMove))
    in
	helper(deck, mv, [])
    end

fun careful_player (deck, goal) = 
    let
	fun helper (deck_hd::deck_tl, mv, hold) = if sum_cards(deck_hd::hold) > goal
						  then helper(deck_tl, mv@[Drop])
    in
    end*)
