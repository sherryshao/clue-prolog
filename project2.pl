% 2013W1 CPSC 312 101
% Project #2
% Eric Chu (73265092) & Sherry Shao (60135084)

% start: Sets player character, number of players and player turn.
start :- 
	% setup valid cards
	write('To start off, please enter all the available cards there are in this board game!'), nl, nl,
	getitems(suspect), nl, getitems(room), nl, getitems(weapon), nl,
	
	% set number of players
	write('How many players are there this round?'), nl,
	read(Num), asserta(numPlayers(Num)), nl,
	
	% set turn number for player
	write('Which turn are you? example: 2'), nl,
	read(Turn), asserta(iam(Turn)), nl,

	% set number of cards for each player
	getnumcards(1),

	write('It is my pleasure to be of your assistance! You are now set as '), write('Player '), write(Turn),
	write(' in a game of '), write(Num), write('.'), nl, nl,
	gethand.

% getnumcards: Inputs other players number of cards
getnumcards(Num) :- 
	numPlayers(NumPlayers), 
	( Num > NumPlayers -> true
	; write('Please enter the number of cards for Player '), write(Num), nl,
	  read(NumCards), assertz(numCards(Num, NumCards)),
	  NewNum is Num + 1, getnumcards(NewNum), nl
	).

% gethand: Inputs player cards.
gethand :- 
	write('Please enter a card in your hand or type \'done\', example: mustard, ballroom, reolver, etc.'), nl,
	read(Card),
	( Card \= done -> 
		% check for invalid cards
		( possible(Card) -> iam(MeId),setOnhand(MeId, Card) , nl, gethand
		; nl, write('That was not a valid card.'), nl, gethand
		)
	; gameplay(1)
	).

% getitems: Inputs valid suspects, rooms, and weapons
getitems(Type) :- 
	write('Please enter a '), write(Type), write(' or type \'done\''), nl,
	read(Card),
	( Card \= done -> 
		% check for existing cards
		( possible(Card) -> nl, write('You have already entered this card.'), nl, getitems(Type)
		; assertz(possible(Card)),
		  ( Type = suspect -> assertz(suspect(Card))
		  ; Type = room -> assertz(room(Card))
		  ; Type = weapon -> assertz(weapon(Card))
		  ), getitems(Type)
		)
	; true
	).

% gameplay(Num): Starts the game at turn Num
gameplay(Num) :-
	nl,nl,
	( iam(Num) ->
		% Player's turn
		write('It is our turn!'), nl,

		% Check if accusation is possible (a valid combination + only 3 possible objects left)
		( possible(X),suspect(X),possible(Y),weapon(Y),possible(Z),room(Z),aggregate_all(count, possible(_), Count),Count is 3 ->
		  write('Good news! I have solved the case, good sir! Please make the following accusation:'), nl,
		  write('Suspect: '), write(X), write(', Weapon: '), write(Y), write(', Room: '), write(Z), nl, nl

		% Check if suggestion is possible (a valid combination)
		; possible(X),suspect(X),possible(Y),weapon(Y),possible(Z),room(Z) -> 
		  write('Here is your next suggestion if you are available to give one: '), nl,
		  write('Suspect: '), write(X), write(', Weapon: '), write(Y), write(', Room: '), write(Z), nl, nl

		% Check if something went wrong and we've removed way too many clauses
		; write('Well this is awkward, I cannot find any more possible combinations... I am afraid you are on your own.'), nl, nl
		),

		% Player choose to accuse, suggest or skip your turn
		write('Please enter \'a\' for accusation, \'s\' for suggestion or \'skip\' to skip your turn.'), nl, nl,
		read(Input),
		( Input \= skip ->
			write('Please enter a suspect: '), nl, read(Suspect), nl,
			write('Please enter a weapon: '), nl, read(Weapon), nl,
			write('Please enter a room: '), nl, read(Room), nl,

			% Player chose to make an accusation
			( Input = a ->
				write('Was the result of our deduction correct? (y/n)'), nl, nl, read(AccusationResult),
				( AccusationResult = y -> win
				; AccusationResult = n -> lose
				)

			% Player chose to make a suggestion
			; Input = s ->
				write('Please enter a card if you were shown one, otherwise, enter \'none\', example: mustard, ballroom, reolver, etc.'), nl,
				read(SuggestionResult), nl,

				% If player was shown a card, remove it from the possible deck; loop
				( SuggestionResult \= none ->
					write('Which player showed you this card? example: 2'), nl, read(PlayerId),setOnhand(PlayerId, SuggestionResult)
				; true
				),

				% Ask if player wants to make an accusation
				write('Would you like to make an accusation? (y/n)'), nl, read(Input2),
				( Input2 = y -> 
					write('Was the result of our deduction correct? (y/n)'), nl, nl, read(AccusationResult2),
					( AccusationResult2 = y -> win
					; AccusationResult2 = n -> lose
					)
				; nextNum(Num, NewNum), gameplay(NewNum)
				)
			)
		; nextNum(Num, NewNum), gameplay(NewNum)
		)

	% Other players' turns
	; write('It is Player '), write(Num), write('\'s turn'), nl,
	  write('Has anyone figured it out yet? '), nl,

	  % Player lets assistant know if other players have made accusations or suggestions
	  write('Please enter \'a\' for accusation, \'s\' for suggestion or \'skip\' to skip their turn.'), nl,
	  read(Input),
		( Input \= skip ->
			write('Please enter a suspect: '), read(Suspect), nl,
			write('Please enter a room: '), read(Room), nl,
			write('Please enter a weapon: '), read(Weapon), nl,

			% If other players made an accusation (we lose if they were correct, else loop)
			( Input = a ->
				write('Was the result of their deduction correct? (y/n)'), nl, read(AccusationResult),
				( AccusationResult = y -> lose
				; AccusationResult = n -> nextNum(Num, NewNum), gameplay(NewNum)
				)

			% If other players made a suggestion
			; Input = s ->
				write('Was a card shown after their suggestion? (y/n)'), nl, read(SuggestionResult), nl,

				% Do something if a card is shown
				( SuggestionResult = y ->
					write('Which player showed them this card? example: 2'), nl, read(CardShowerId), nl, 
					makeInference(CardShowerId, Suspect, Room, Weapon)
				; true
				),

				% Check if other players made an accusation right after their suggestion
				write('Did player '), write(Num), write(' make an accusation right after the latter suggestion? (y/n)'), nl, read(Input2),
				( Input2 = y ->
					write('Was the result of their deduction correct? (y/n)'), nl, read(AccusationResult2),
					( AccusationResult2 = y -> lose
					; AccusationResult2 = n -> nextNum(Num, NewNum), gameplay(NewNum)
					)
				; Input2 = n ->
					( SuggestionResult = n -> inferUnrejectedSuggestion(Num, Suspect, Room, Weapon),nextNum(Num, NewNum), gameplay(NewNum) 
					; SuggestionResult = y -> nextNum(Num, NewNum), gameplay(NewNum)
					)
				; nextNum(Num, NewNum), gameplay(NewNum)
				)
			)
		; nextNum(Num, NewNum), gameplay(NewNum)
		)
	).

% nextNum(Num, NewNum): Increment Num into NewNum
nextNum(Num,NewNum) :- numPlayers(Num),NewNum is 1.
nextNum(Num,NewNum) :- NewNum is Num + 1.

lose :- write('My deepest apologies. We have lost this battle.').
win :- write('Congratulations, results show that we have conquered this majestic game of Clue!').

% makeInference(Num, Card(Suspect), Card(Room), Card(Weapon)) : makes inference based on what we already know
% if a player is able to reject a set of cards when 2 of which we know are held by other players, we can make
% conclusions about which card was shown (therefore this player has) and record this information down in the form of onhand(playerId, Card)
makeInference(CardShowerId, Suspect, Room, Weapon) :- 
	onhand(CardShowerId, Suspect);onhand(CardShowerId, Room);onhand(CardShowerId, Weapon)
	;onhand(_, Suspect),onhand(_, Room),setOnhand(CardShowerId, Weapon)
	;onhand(_, Room),onhand(_, Weapon),setOnhand(CardShowerId, Suspect)
	;onhand(_, Suspect),onhand(_, Weapon),setOnhand(CardShowerId, Room)
	;true.

% setOnhand: records down a Card that is held by Player and render it impossible to be in the envelope
% called when we see or infer this fact
setOnhand(PlayerId, Card) :- possible(Card) -> assert(onhand(PlayerId, Card)),retract(possible(Card))
						; not(possible(Card)) -> assert(onhand(PlayerId, Card)).

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

:- dynamic suspect/1.
:- dynamic room/1.
:- dynamic weapon/1.
:- dynamic possible/1.
:- dynamic onhand/2.




