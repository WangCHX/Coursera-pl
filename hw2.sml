(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (a,lst) = 
    case lst of
	[] => NONE
      | x::xs => if same_string(a,x) then SOME xs
		 else case all_except_option(a,xs) of
				   NONE => NONE
				| SOME i => SOME (x::i)

fun get_substitutions1 (lls,s) = 
    case lls of
	[] => []
       | x::xs => (case all_except_option(s,x) of
			  NONE => []
			| SOME i => i) @ get_substitutions1(xs,s)

fun get_substitutions2 (lls,s) = 
    let 
	fun helper(ls,res) = 
	    case ls of
		[] => res
	      | x::xs => helper(xs,x::res) 
    in
	case lls of
	    [] => []
	  | x::xs => helper((case all_except_option(s,x) of
			  NONE => []
			| SOME i => i), get_substitutions1(xs,s))
    end

fun similar_names (lls,{first=x,middle=y,last=z}) =
    let 
	val xs = get_substitutions2(lls,x)
	fun convert(ls) = 
	    case ls of
		[] => []
	      | l::ls' => {first = l,middle = y,last = z}::convert(ls')
    in
	{first=x,middle=y,last=z}::convert(xs)
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
fun card_color card =
    case card of
	(Clubs,_) => Black
      | (Spades,_) => Black
      | (Diamonds,_) => Red
      | (Hearts,_) => Red

fun card_value card  =
    case card of
	(_,Num i) => i
      | (_,Ace) => 11
      | (_,Jack) => 10
      | (_,Queen) => 10
      | (_,King) => 10
  
fun remove_card (cs,c,ex) =
    case cs of
	[] => raise ex
      | x::xs => if x = c then xs else x::remove_card(xs,c,ex)



fun all_same_color ls = 
    case ls of
	[] => true
      | _::[] => true
      | head::(neck::rest) => ((card_color head = card_color neck) andalso all_same_color(neck::rest))

fun sum_cards ls = 
    let
	fun helper(xs,sum) =
	    case xs of
		[] => sum
	      | l::ls => helper(ls,sum + card_value l) 
    in
	helper(ls,0)
    end

fun score (ls, goal) = 
    let
	val sum = sum_cards ls
	val sc = all_same_color ls
    in
	case sum > goal of
	    true => if sc then 3*( sum - goal) div 2 else 3 * (sum - goal)
	  | false => if sc then (goal - sum) div 2 else goal - sum
    end

fun officiate (ls, move, goal) = 
    let
	fun helper(sc,held,x,m) = 
	    case m of
		[] => score (held, goal) 
	      | a::as' => case a of
			     Discard i => helper(sc - card_value i,remove_card(held,i,IllegalMove),x,as')
			   | Draw => case x of
					 [] => score (held, goal)
				       | y::ys => if (sc + card_value y) > goal then score (y::held, goal) 
						  else
						      helper(sc + card_value y, y::held,ys,as')
    in
	helper (0,[],ls,move) 
    end
(*
fun score_challenge ()
*)
