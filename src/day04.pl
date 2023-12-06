:- module(day04, [day04/2]).

:- use_module(library(assoc)).
:- use_module(library(between)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(debug)).
:- use_module(library(ordsets)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(parsing).

numbers([]) -->
  [].
numbers(Numbers) -->
  " ", numbers(Numbers).
numbers([Number]) -->
  pos_integer(Number).
numbers([Number | Numbers]) -->
  pos_integer(Number), " ", numbers(Numbers).

card(card(Id, WinningNumbers, OwnedNumbers)) -->
  "Card", many1(' '), pos_integer(Id), ": ", numbers(WinningNumbers0), " | ", numbers(OwnedNumbers0), "\n",
  { list_to_ord_set(WinningNumbers0, WinningNumbers), list_to_ord_set(OwnedNumbers0, OwnedNumbers) }.

cards([]) -->
  [].
cards([Card | Cards]) -->
  card(Card), cards(Cards).

card_score(card(_, Winning, Owned), Score) :-
  ord_intersection(Winning, Owned, Won),
  length(Won, L),
  ( L #> 0
  ->  Score #= 2 ^ (L - 1)
  ;   Score #= 0
  ).

cards_number_card(Cards, Number, card(Number, Winning, Owned)) :-
  member(card(Number, Winning, Owned), Cards).

card_copies(card(Number, Winning, Owned), Copies) :-
  ord_intersection(Winning, Owned, Won),
  length(Won, L),
  End #= Number + L,
  numlist(Number, End, [_ | Copies]).

copies_number_count(Copies, Number, Count) :-
  maplist(\Ns^B^(member(Number, Ns) -> B #= 1; B #= 0), Copies, ReversedBits),
  reverse(ReversedBits, Bits),
  integer_binary(I, Bits),
  Count #= 1 + I.

day04(Part1, Part2) :-
  phrase_from_file(cards(Cards), "../data/day04.txt"),
  *part1(Cards, Part1),
  part2(Cards, Part2).

part1(Cards, Part1) :-
  maplist(card_score, Cards, Scores),
  sum_list(Scores, Part1).

part2(Cards, Part2) :-
  maplist(card_copies, Cards, Copies),
  length(Copies, L),
  numlist(L, Numbers),
  maplist(copies_number_count(Copies), Numbers, Counts),
  sum_list(Counts, Part2).
