(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (s,xs) =
  case xs of
      [] => NONE
    | x::xs' => if same_string(s,x)
                then SOME xs'
                else case all_except_option(s,xs') of
                         NONE => NONE
                       | SOME y => SOME(x::y)

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

fun card_value card =
    case card of
        (_, Num rank) => rank
        | (_, Ace) => 11
        | _ => 10

fun remove_card (cards, card, e) =
    case cards of
        [] => raise e
        | head::tail => if head = card then tail else remove_card(tail, card, e)

fun all_same_color cards =
    case cards of
        head::neck::tail => if card_color head = card_color neck then all_same_color(neck::tail) else false
        | _ => true

fun sum_cards cards =
    let
        fun aux (list, res) =
            case list of
                [] => res
                | head::tail => aux(tail, res + card_value head)
    in
        aux(cards, 0)
    end

fun score (cards, goal) =
    let
        val sum = sum_cards(cards)
        val preliminary = if sum > goal then 3 * (sum - goal) else goal - sum
    in
        case all_same_color(cards) of
            true => preliminary div 2
            | false => preliminary
    end
    
fun officiate (draw_pile, move_list, goal) =
    let
        fun turn (moves, draws, hand) =
            case (moves, draws, hand) of
                ([], _, _) => score(hand, goal)
                | (Draw::moves', [], hand) => score(hand, goal)
                | (Draw::moves', next::draw', hand) =>
                    if sum_cards(next::hand) > goal then score(next::hand, goal) else turn(moves', draw', next::hand)
                | (Discard card::moves', draw, hand) => turn(moves', draw, remove_card(hand, card, IllegalMove))
    in
        turn(move_list, draw_pile, [])
    end