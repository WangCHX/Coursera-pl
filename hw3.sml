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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals list = List.filter (fn x => Char.isUpper (String.sub(x, 0))) list

fun longest_string1 list = foldl (fn (x,y) => if String.size x > String.size y then x else y) "" list

fun longest_string2 list = foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" list


fun longest_string_helper f =
    fn ls => foldl (fn (x, y) => if f(String.size x,String.size y) then x else y) "" ls


val longest_string3  = longest_string_helper (fn (x, y) => x > y )
val longest_string4  = longest_string_helper (fn (x, y) => x >=y )

val longest_capitalized = longest_string1 o only_capitals

val rev_string = implode o rev o explode

fun first_answer f ls = 
       case ls of
	   [] => raise NoAnswer
	 | x::xs => case f x of
			NONE => first_answer f xs
		      | SOME v => v 


fun all_answers f ls = 
    let
	fun helper acc es =
	       case es of
		   [] => SOME acc
		 | x::xs => case f x of
				NONE => NONE
			      | SOME v => helper (acc @ v) xs
    in
	helper [] ls
    end

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

fun count_wildcards p = g (fn _ => 1) (fn x => 0) p
fun count_wild_and_variable_lengths p = g (fn _ => 1) (fn x => String.size x) p
fun count_some_var (s, p) = g (fn _ => 0) (fn x => if s = x then 1 else 0) p

fun check_pat p =
    let
	fun getStr pa = 
	    case pa of
		Wildcard => []
	      | Variable x => [x] 
	      | TupleP ps => List.foldl (fn (p, i) => (getStr p) @ i) [] ps
	      | ConstructorP(_, p) => getStr p
	      | _ => [] 
	fun isDiff ls = 
	    case ls of
		[] => true
	      | x::xs => if List.exists (fn a => a = x) xs then false else isDiff xs 
    in
	isDiff (getStr p)
    end

fun match (v, p) = 
    case (v, p) of
	(_, Wildcard) => SOME []
      | (_, Variable s) => SOME [(s,v)]
      | (Unit, UnitP) => SOME []
      | (Const a, ConstP b) => if a = b then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length ps = List.length vs then all_answers (fn x => match (#1 x, #2 x)) (ListPair.zip (vs, ps)) else NONE
      | (Constructor(s1,v), ConstructorP(s2,p)) => if s1 = s2 then match(v, p) else NONE 
      | (_, _)  => NONE  

fun first_match v =
    fn pl => SOME (first_answer (fn x => match(v, x)) pl)
       handle NoAnswer => NONE

fun get_pattern_type (type_data ,pattern) =
  let 
      fun check_type_data (type_data1, datatype_str,cons_type) =
	  case type_data1 of
              []=> raise NoAnswer
             |(x,y,z)::xs => if x = datatype_str
                             then Datatype y
                             else check_type_data(xs, datatype_str,cons_type)
					     
      fun helper ptn =
	  case  ptn of
	      [] => []  
	     |x::xs  => (get_pattern_type(type_data, x))::helper xs
  in
  
      case pattern of
	  Wildcard => Anything
	 |UnitP => UnitT
	 |ConstP v => IntT
	 |TupleP v =>  TupleT(helper v)
	 |Variable v1 =>Anything
	 |ConstructorP(s,v)=>check_type_data (type_data, s, get_pattern_type(type_data,v)) 
end


 fun get_most_lenient  (typ1 ,typ2) =
     let 
         fun helper typelst =
             case  typelst of
                 ([],[]) => []  
                |(x::xs,y::ys)  => (get_most_lenient  (x,y))::helper(xs,ys)
                |_ => raise NoAnswer
     in
         case (typ1,typ2) of
             (Anything,_)=>typ2
            |(Datatype s1,Datatype s2) => if s1 = s2 then typ1 (*typ1 == typ2*) else raise NoAnswer
            |(IntT,IntT)=>IntT
            |(UnitT,UnitT)=>UnitT
            |(TupleT v1,TupleT v2) => TupleT(helper(v1,v2))
            |(_, Anything) =>typ1
            |(_,_)=> raise NoAnswer
     end
	 

fun typecheck_patterns (type_data, pattern_list) = 
    if null pattern_list
    then NONE
    else
	SOME (List.foldl (fn(x,acc) => get_most_lenient ( get_pattern_type(type_data, x),acc) ) Anything pattern_list)
	handle NoAnswer => NONE
