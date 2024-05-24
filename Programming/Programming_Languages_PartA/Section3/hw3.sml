exception NoAnswer

fun only_capitals xs =
	List.filter (fn s => Char.isUpper(String.sub(s,0))) xs


fun longest_string1 xs =
	foldl (fn (x,y) => if String.size x > String.size y then x else y) "" xs

fun longest_string2 xs =
	foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" xs

fun longest_string_helper  f xs =
	foldl (fn (x,y) => if f (String.size x,String.size y) then x else y) "" xs


val longest_string3 = longest_string_helper (fn(x,y) => x > y)

val longest_string4 = longest_string_helper (fn(x,y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string =
  implode o rev o explode

 fun first_answer  f xs =
 	case xs of
 		[] =>raise NoAnswer
 		| x::xs' => case f x of
		 			SOME v => v
		 			| NONE => first_answer f xs'

fun all_answers f xs =
	let fun helper (acc,xs) =
		case xs of 
			[] => SOME acc
			| x::xs' => case f x of
						NONE => NONE
						| SOME v => helper(v @ acc,xs')
	in
		helper([],xs)
	end

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


fun count_wildcards p =
    g (fn () => 1) (fn x => 0) p


fun count_wild_and_variable_lengths p =
    g (fn() => 1) (fn s => String.size s) p

fun count_some_var (s,p) =
    g (fn() => 0) (fn str => if str = s then 1 else 0) p



fun check_pat p = 
    let
        fun all_variables p = case p of
                                Variable x  => [x]
                              | TupleP xs   => List.foldl (fn (p, acc) => all_variables(p) @ acc) [] xs
                              | ConstructorP(_, p) => all_variables p
                              | _ => []
        
        
        fun repeat xs =
          case xs of
            [] => false
          | x::xs' => List.exists (fn y => y = x) xs' orelse repeat xs' 
    in
        not ( repeat( all_variables p ) )
    end

fun match (v, p) =
	case (v, p) of
		(_, Wildcard) => SOME []
		| (v, Variable x) => SOME [(x, v)]
		| (Unit, UnitP) => SOME []
		| (Const x, ConstP y) => if x = y then SOME [] else NONE
		| (Tuple vs, TupleP ps) => if List.length vs = List.length ps
								   then all_answers (fn (v, p) => match(v, p)) (ListPair.zip(vs, ps))
								   else NONE
        |(Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2 
	        									     then match (v, p)
	        									     else NONE
        | _ => NONE


fun first_match v ps =
	SOME (first_answer (fn p => match(v, p)) ps)
	handle NoAnswer => NONE
