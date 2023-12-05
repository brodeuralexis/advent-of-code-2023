:- module(day01, [day01/2]).

:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(dif)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(tabling)).
:- use_module(parsing).

line([], _) -->
  eol.
line(['1' | Cs], true), "e" -->
  "one", line(Cs, true).
line(['2' | Cs], true), "o" -->
  "two", line(Cs, true).
line(['3' | Cs], true), "e" -->
  "three", line(Cs, true).
line(['4' | Cs], true) -->
  "four", line(Cs, true).
line(['5' | Cs], true), "e" -->
  "five", line(Cs, true).
line(['6' | Cs], true) -->
  "six", line(Cs, true).
line(['7' | Cs], true), "n" -->
  "seven", line(Cs, true).
line(['8' | Cs], true), "t" -->
  "eight", line(Cs, true).
line(['9' | Cs], true), "e" -->
  "nine", line(Cs, true).
line([C | Cs], Alt) -->
  [C], { dif(C, '\n'), char_type(C, decimal_digit) }, !, line(Cs, Alt).
line(Cs, Alt) -->
  [C], { dif(C, '\n'), \+ char_type(C, decimal_digit) }, !, line(Cs, Alt).

lines([Line], Alt) -->
  line(Line, Alt).
lines([Line | Lines], Alt) -->
  line(Line, Alt), lines(Lines, Alt).

day01(_, Part2) :-
  % phrase_from_file(lines(Lines1, false), "../data/day01.txt"),
  phrase_from_file(lines(Lines2, true), "../data/day01.txt"),
  lines_sum(Lines2, Part2).

list_tails([H | Ls], [H, T]) :-
  reverse([H | Ls], [T | _]).

% :- table lines_sum/2.
lines_sum(Lines, Sum) :-
  maplist(list_tails, Lines, RawNumbers),
  maplist(number_chars, Numbers, RawNumbers),
  sum_list(Numbers, Sum).
