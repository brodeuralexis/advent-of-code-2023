:- module(day05, [day05/2]).

:- use_module(library(between)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(parsing).

integers([I | Is]) -->
  integer(I), " ", integers(Is).
integers([I]) -->
  integer(I).

range(range(DestinationStart-DestinationEnd, SourceStart-SourceEnd)) -->
  integer(DestinationStart), " ", integer(SourceStart), " ", integer(Length),
  { DestinationEnd #= DestinationStart + Length - 1, SourceEnd #= SourceStart + Length - 1 }.

ranges([]) -->
  [].
ranges([Range | Ranges]) -->
  range(Range), "\n", ranges(Ranges).

almanac(almanac(Seeds, [SeedToSoil, SoilToFertilizer, FertilizerToWater, WaterToLight, LightToTemperature, TemperatureToHumidity, HumidityToLocation])) -->
  "seeds: ", integers(Seeds), "\n",
  "\n",
  "seed-to-soil map:\n",
  ranges(SeedToSoil), "\n",
  "soil-to-fertilizer map:\n",
  ranges(SoilToFertilizer), "\n",
  "fertilizer-to-water map:\n",
  ranges(FertilizerToWater), "\n",
  "water-to-light map:\n",
  ranges(WaterToLight), "\n",
  "light-to-temperature map:\n",
  ranges(LightToTemperature), "\n",
  "temperature-to-humidity map:\n",
  ranges(TemperatureToHumidity), "\n",
  "humidity-to-location map:\n",
  ranges(HumidityToLocation).

ranges_source_range(Ranges, Source, Range) :-
  member(Range, Ranges),
  Range = range(_, SourceStart-SourceEnd),
  between(SourceStart, SourceEnd, Source).

overlaps(R1s-_, R2s-R2e) :- between(R2s, R2e, R1s).
overlaps(_-R1e, R2s-R2e) :- between(R2s, R2e, R1e).
overlaps(R1s-R1e, R2s-_) :- between(R1s, R1e, R2s).
overlaps(R1s-R1e, _-R2e) :- between(R1s, R1e, R2e).

ranges_source_destination(Ranges, Source, Source) :-
  \+ ranges_source_range(Ranges, Source, _).
ranges_source_destination(Ranges, Source, Destination) :-
  ranges_source_range(Ranges, Source, range(DestinationStart-_, SourceStart-_)),
  Delta #= Source - SourceStart,
  Destination #= DestinationStart + Delta.

almanac_seed_location(almanac(_, [SeedToSoil, SoilToFertilizer, FertilizerToWater, WaterToLight, LightToTemperature, TemperatureToHumidity, HumidityToLocation]), Seed, Location) :-
  ranges_source_destination(SeedToSoil, Seed, Soil),
  ranges_source_destination(SoilToFertilizer, Soil, Fertilizer),
  ranges_source_destination(FertilizerToWater, Fertilizer, Water),
  ranges_source_destination(WaterToLight, Water, Light),
  ranges_source_destination(LightToTemperature, Light, Temperature),
  ranges_source_destination(TemperatureToHumidity, Temperature, Humidity),
  ranges_source_destination(HumidityToLocation, Humidity, Location).

almanac_result_mapping(Almanac, Locations, Mapping) :-
  Almanac = almanac(Seeds, _),
  maplist(call(Mapping, Almanac), Seeds, Locations).

pairs_numbers([], []).
pairs_numbers([Start, Length | Pairs], Numbers) :-
  append(Numbers0, Numbers1, Numbers),
  End #= Start + Length - 1,
  numlist(Start, End, Numbers0),
  pairs_numbers(Pairs, Numbers1).

in(Almanac) :-
  phrase_from_file(almanac(Almanac), "../data/day05.txt").

day05(Part1, Part2) :-
  in(Almanac),
  part1(Almanac, Part1),
  *part2(Almanac, Part2).

part1(Almanac, Part1) :-
  almanac_result_mapping(Almanac, Locations, almanac_seed_location),
  list_min(Locations, Part1).

part2(Almanac0, Part2) :-
  Almanac0 = almanac(Seeds0, Maps),
  Almanac = almanac(Seeds, Maps),
  pairs_numbers(Seeds0, Seeds),
  almanac_result_mapping(Almanac, Locations, almanac_seed_location),
  list_min(Locations, Part2).
