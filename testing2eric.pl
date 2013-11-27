:- dynamic suspect/1.
:- dynamic room/1.
:- dynamic weapon/1.
:- dynamic possible/1.
:- dynamic onhand/2.

onhand(3, plum).
onhand(3, scarlet).
onhand(1, kitchen).
onhand(2, lounge).
onhand(3, candlestick).
onhand(6, knife).
onhand(0, hall).

numCards(1, 3).
numCards(2, 3).
numCards(3, 3).
numCards(4, 2).

countPossibles(Type, Count) :- 
	( Type = suspect -> aggregate_all(count, possibles(suspect), Count)
	; Type = room -> aggregate_all(count, possibles(room), Count)
	; Type = weapon -> aggregate_all(count, possibles(weapon), Count)
	).

possibles(Type) :-
	( Type = suspect -> possible(X),suspect(X)
	; Type = room -> possible(X),room(X)
	; Type = weapon -> possible(X),weapon(X)
	).

% Suspects 

suspect(plum).
suspect(scarlet).
suspect(mustard).
suspect(green).
suspect(white).
suspect(peacock).

% Rooms

room(kitchen).
room(ballroom).
room(conservatory).
room(billiard).
room(library).
room(study).
room(hall).
room(lounge).
room(dining).

% Weapons

weapon(knife).
weapon(candlestick).
weapon(revolver).
weapon(rope).
weapon(pipe).
weapon(wrench).

% Current Possible Objects

possible(plum).
possible(scarlet).
possible(mustard).
%possible(green).
possible(white).
possible(peacock).

possible(knife).
possible(candlestick).
possible(revolver).
possible(rope).
possible(pipe).
possible(wrench).

possible(kitchen).
possible(ballroom).
possible(conservatory).
possible(billiard).
possible(library).
possible(study).
possible(hall).
possible(lounge).
possible(dining).