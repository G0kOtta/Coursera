fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(str:string,str_list:string list) =
    case str_list of
        [] => NONE
        | x::xs =>  if (same_string(x,str))
                    then SOME xs
                    else case all_except_option(str,xs) of
                        NONE => NONE
                        | SOME value => SOME (x::value)

fun get_substitutions1(str_ll,str) =
    case str_ll of
        [] => []
        | x::xs => case all_except_option(str,x) of
                   NONE => get_substitutions1(xs,str)
                   | SOME value => value @ get_substitutions1(xs,str)

fun get_substitutions2(xs,s) =
    let fun helper (xs,s,acc) =
        case xs of
            [] => acc
            | x::xs' =>case all_except_option(s,x) of
                NONE => helper(xs',s,acc)
                | SOME value => helper(xs',s,value @ acc)
        in 
            helper(xs,s,[])
        end

fun similar_names (str_ll,name) = 
    let val {first=x1,middle=x2,last=x3} = name
        fun helper xs =
            case xs of
                [] => []
                | x::xs' =>{first=x,middle=x2,last=x3}::helper xs'
        in
            name::helper(get_substitutions1(str_ll,x1))
        end


datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color(suit,rank) =
    case suit of
        Clubs => Black
        | Spades => Black
        | Diamonds => Red
        | Hearts => Red

fun card_value(suit,rank) =
    case rank of
        Num i => i
        | Ace => 11
        | _ =>10

fun remove_card(card_list,card,exn) =
    case card_list of
        [] => raise exn
        | x::xs => if (x = card) then xs else remove_card(xs,card,exn)


fun all_same_color(card_list) =
    case card_list of
        [] => true
        | x::xs => case xs of
                        [] => true
                        | y::xs' => if (card_color(x) = card_color(y))
                                  then all_same_color(xs)
                                  else false


fun sum_cards(card_list) =
    case card_list of
        [] => 0
        | x::xs => card_value(x) + sum_cards(xs)

fun score(card_list,goal) =
    let val sum = sum_cards(card_list)
        fun pre_score(sum,goal) = 
            if(sum > goal)
            then 3 * (sum-goal)
            else goal - sum
        in
            if (all_same_color(card_list))
            then pre_score(sum,goal) div 2
            else pre_score(sum,goal)
        end


fun officiate (card_list, moves, goal) =
    let
        fun play(card_list, current_helds, remain_moves) =
            case remain_moves of
               [] => current_helds
             | head::tail => case head of
                Discard c => play(card_list, remove_card(current_helds, c, IllegalMove), tail)
              | Draw => case card_list of
                 [] => current_helds
               | c::cs => 
                    if sum_cards (c::current_helds) > goal
                    then c::current_helds
                    else play(cs, c::current_helds, tail)
    in
        score (play(card_list,[], moves), goal)
    end