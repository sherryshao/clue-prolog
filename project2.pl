% 2013W1 CPSC 312 101
% Project #2
% Eric Chu (73265092) & Sherry Shao (60135084)

% start: Sets player character, number of players and player turn.
start :- 
	% set player's character
	write('Who is your character?'), nl,
	read(Player), 
	% check for invalid characters
	( suspect(Player) -> asserta(iam(Player)), nl
	; nl, write('That was not a valid character.'), nl, start
	),
	% set number of players
	write('How many players are there this round?'), nl,
	read(Num), asserta(numPlayers(Num)), nl,
	% set turn number for player
	write('Which turn are you? example: 2'), nl,
	read(Turn), asserta(turn(Turn)), nl,
	write('It is my pleasure to be of your assistance! You are now set as '), write(Player), 
	write(', Player '), write(Turn),
	write(', in a game of '), write(Num), write('.'), nl, nl,
	gethand.

% gethand: Inputs player cards.
gethand :- 
	write('Please enter a card or type \'done\', example: mustard, ballroom, reolver, etc.'), nl,
	read(Card),
	( Card \= done -> 
		% check for invalid cards
		( possible(Card) -> retract(possible(Card)),iam(MeId),asserta(onhand(MeId, Card)) , nl, gethand
		; nl, write('That was not a valid card.'), nl, gethand
		)
	; gameplay(1)
	).

% gameplay(Num): Starts the game at turn Num
gameplay(Num) :-
	nl,nl,
	( turn(Num) ->
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

		% Check.. if something went wrong and we've removed way too many clauses
		; write('Well this is awkward, I cannot find any more possible combinations... I am afraid you are on your own.'), nl, nl
		),

		% Player choose to accuse, suggest or skip in their turn
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
				write('Which player showed you this card? example: 2'), nl,
				read(PlayerId),

				% If player was shown a card, remove it from the possible deck; loop
				( SuggestionResult \= none ->
					retract(possible(SuggestionResult)), asserta(onhand(PlayerId, SuggestionResult)), nextNum(Num, NewNum), gameplay(NewNum)
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
				; AccusationResult = n -> assert((impossibleSet(Suspect, Room, Weapon))),resetPossibles,nextNum(Num, NewNum), gameplay(NewNum)
				)

			% If other players made a suggestion
			; Input = s ->
				write('Was a card shown after their suggestion by a player other than yourself? (y/n)'), nl, read(SuggestionResult),

				% Do something if a card is shown
				( SuggestionResult = y -> assert((impossibleSet(Suspect, Room, Weapon))),resetPossibles,nextNum(Num, NewNum), gameplay(NewNum)
				; SuggestionResult = n -> nextNum(Num, NewNum), gameplay(NewNum)
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

resetPossibles :- findall(X,possible(X),Possibles),removeImpossibles(Possibles).
 
removeImpossible(H) :- 
			possible(H),suspect(H),impossibleSet(H, X, Y),onhand(X),onhand(Y),retract(possible(H)),retract(impossibleSet(H, X, Y));
			possible(H),room(H),impossibleSet(X, H, Y),onhand(X),onhand(Y),retract(possible(H)),retract(impossibleSet(X, H, Y));
			possible(H),weapon(H),impossibleSet(X, Y, H),onhand(X),onhand(Y),retract(possible(H)),retract(impossibleSet(X, Y, H)).

removeImpossibles(Fl) :- removeImpossiblesHelper(Fl, Fl).

removeImpossiblesHelper([], _).
removeImpossiblesHelper([H|L], Fl) :- 	removeImpossible(H),removeImpossiblesHelper(Fl, Fl);
										not(removeImpossible(H)),removeImpossiblesHelper(L, Fl).

% Setup objects

:- dynamic suspect/1.
:- dynamic weapon/1.
:- dynamic room/1.
:- dynamic possible/1.

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

