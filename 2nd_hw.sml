(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str, list) =
    let fun aux (list', exists) = 
        case list' of
            [] => ([], exists)
            | head::tail => 
                if same_string(head, str)
                then aux(tail, true)
                else let val (rest_list, rest_exists) = aux(tail, exists) in (head::rest_list, rest_exists) end
    in
        case aux(list, false) of
            (_, false) => NONE
            | (list, _) => SOME list
    end

fun get_substitutions1 (list, str) =
    case list of
        [] => []
        | head::tail =>
            case all_except_option(str, head) of
                NONE => get_substitutions1(tail, str)
                | SOME list' => list' @ get_substitutions1(tail, str)

fun get_substitutions2 (list, str) =
    let
        fun aux (list, res) =
            case list of
                [] => res
                | head::tail =>
                    case all_except_option(str, head) of
                        NONE => aux(tail, res)
                        | SOME list' => aux(tail, res @ list')
    in
        aux(list, [])
    end

fun similar_names (substitutions, { first, middle, last }) =
    let
        val possible_subs = get_substitutions2(substitutions, first)
        fun substitute_firsts (subs) =
            case subs of
                [] => []
                | sub::tail => { first=sub, middle=middle, last=last }::substitute_firsts(tail)
    in
        { first=first, middle=middle, last=last }::substitute_firsts(possible_subs)
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
        (Diamonds, _) => Red
        | (Hearts, _) => Red
        | _ => Black
