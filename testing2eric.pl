:- dynamic suspect/1.
:- dynamic room/1.
:- dynamic weapon/1.
:- dynamic possible/1.
:- dynamic onhand/2.


onhand(3, scarlet).
onhand(1, kitchen).
onhand(2, lounge).
onhand(3, candlestick).
onhand(6, knife).
onhand(3, hall).

numCards(1, 3).
numCards(2, 3).
numCards(3, 3).
numCards(4, 2).

% inferUnrejectedSuggestion(): when a suggestion has been made, unrejected, but the player did not proceed to make an accusation
% if we know all the cards that they have on hand, we can infer that whatever that is not on their hand is in the envelop
inferUnrejectedSuggestion(PlayerId, Suspect, Room, Weapon) :- 
	numCards(PlayerId, NumCards),aggregate_all(count, onhand(PlayerId, _), Count),
	( Count is NumCards ->
		( not(onhand(PlayerId, Suspect)) -> conclude(suspect, Suspect)
		, not(onhand(PlayerId, Room)) -> conclude(room, Room)
		, not(onhand(PlayerId, Weapon)) -> conclude(weapon, Weapon)
		)
	)
	; true.

% conclude(): make conclusion about a type by removing all remaing possible predicates of the given type other than the one indicated
conclude(Type, Card) :- 
	( Type = suspect -> findall(X, suspect(X), Suspects),removeAllExcept(Card, Suspects)
	; Type = room -> findall(X, room(X), Rooms),removeAllExcept(Card, Rooms)
	; Type = weapon -> findall(X, weapon(X), Weapons),removeAllExcept(Card, Weapons)
	).

% removeAllExcept(): helper function for conclude()
removeAllExcept(Card, []) :- asserta(possible(Card)).
removeAllExcept(Card, [H|T]) :- 
	possible(H) -> retract(possible(H)),removeAllExcept(Card,T)
	; not(possible(H)) -> removeAllExcept(Card, T).


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
possible(green).
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