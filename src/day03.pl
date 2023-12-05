:- module(day03, [day03/2]).

:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(debug)).
:- use_module(library(dif)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(pio)).
:- use_module(library(reif)).
:- use_module(parsing).

line([]) -->
  "\n".
line([C | Cs]) -->
  [C], { dif(C, '\n') }, line(Cs).

lines([]) -->
  [].
lines([L | Ls]) -->
  line(L), lines(Ls).

schematic([H | T]) -->
  lines([H | T]), { maplist(same_length(H), T) }.

schematic_position_value([H | T], X-Y, Value) :-
  length([H | T], YMax),
  length(H, XMax),
  ((Y #>= 0, Y #< YMax) -> nth0(Y, [H | T], Row)),
  ((X #>= 0, X #< XMax) -> nth0(X, Row, Value)).

schematic_position_neighbor(Schematic, X-Y, neighbor(NX-NY, NV)) :-
  NX #>= X - 1, NX #=< X + 1, NY #>= Y - 1, NY #=< Y + 1,
  dif(NX-NY, X-Y), dif(NV, '.'),
  schematic_position_value(Schematic, NX-NY, NV).

schematic_position_digit(Schematic, X-Y, Digit) :-
  schematic_position_value(Schematic, X-Y, Digit),
  char_type(Digit, decimal_digit).

schematic_position_symbol(Schematic, X-Y, Symbol) :-
  schematic_position_value(Schematic, X-Y, Symbol),
  dif(Symbol, '.'),
  \+ char_type(Symbol, decimal_digit).

schematic_position_digits(Schematic, X-Y, Digits) :-
  schematic_position_digit(Schematic, X-Y, Digit),
  schematic_position_digits_(Schematic, X-Y, ReversedBefore, -1),
  reverse(ReversedBefore, Before),
  schematic_position_digits_(Schematic, X-Y, After, 1),
  append([Before, [Digit], After], Digits).
schematic_position_digits_(Schematic, X-Y, [], Delta) :-
  XN #= X + Delta,
  \+ schematic_position_digit(Schematic, XN-Y, _).
schematic_position_digits_(Schematic, X-Y, [Digit | Digits], Delta) :-
  XN #= X + Delta,
  schematic_position_digit(Schematic, XN-Y, Digit),
  schematic_position_digits_(Schematic, XN-Y, Digits, Delta).

schematic_position_number(Schematic, X-Y, Number) :-
  schematic_position_digits(Schematic, X-Y, Digits),
  number_chars(Number, Digits).

schematic_position_neighboringNumbers(Schematic, X-Y, Numbers) :-
  findall(P, (schematic_position_neighbor(Schematic, X-Y, neighbor(P, V)), char_type(V, decimal_digit)), Positions),
  maplist(schematic_position_number(Schematic), Positions, Numbers0),
  list_to_ord_set(Numbers0, Numbers).

day03(Part1, Part2) :-
  phrase_from_file(schematic(Schematic), "../data/day03.txt"),
  part1(Schematic, Part1),
  part2(Schematic, Part2).

part1(Schematic, Part1) :-
  findall(P, schematic_position_symbol(Schematic, P, _), SymbolPositions),
  maplist(schematic_position_neighboringNumbers(Schematic), SymbolPositions, PartNumbers0),
  append(PartNumbers0, PartNumbers),
  sum_list(PartNumbers, Part1).

part2(Schematic, Part2) :-
  findall(P, schematic_position_symbol(Schematic, P, '*'), GearPositions),
  maplist(schematic_position_neighboringNumbers(Schematic), GearPositions, PossibleGearNumbers),
  tfilter(\Ns^B^(length(Ns, L), L #= 2 -> B = true; B = false), PossibleGearNumbers, GearNumbers),
  maplist(\[A, B]^R^(R #= A * B), GearNumbers, GearRatios),
  sum_list(GearRatios, Part2).
