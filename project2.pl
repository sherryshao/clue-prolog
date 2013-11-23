% 2013W1 CPSC 312 101
% Project #2
% Eric Chu (73265092) & Sherry Shao (60135084)

% start: Sets player character, number of players and player turn.
start :- 
	write('Who is your character?'), nl,
	read(Player), asserta(iam(Player)), nl,
	write('How many players are there this round?'), nl,
	read(Num), asserta(numPlayers(Num)), nl,
	write('Which turn are you? example: 2'), nl,
	read(Turn), asserta(turn(Turn)), nl,
	write('It is my pleasure to be of your assistance! You are now set as '), write(Player), 
	write(', Player '), write(Turn),
	write(', in a game of '), write(Num), write('.'), nl, nl,
	gethand.

% gethand: Inputs player cards.
gethand :- 
	write('Please enter a card or type \'done\', example: suspect(mustard)'), nl,
	read(Card),
	( Card \= done -> asserta(Card), nl, gethand
	; gameplay(1)
	).

% gameplay(Num): Starts the game at turn Num
gameplay(Num) :-
	nl,nl,
	( turn(Num) ->
		write('It is our turn!'), nl,

		% Suggest a suggestion / accusation here, for example
		write('Good news! I have solved the case, good sir! Please make the following accusation:'), nl,
		write('Suspect: Scarlet, Room: Dining Room, Weapon: Revolver'), nl, nl,

		write('Please enter \'a\' for accusation, \'s\' for suggestion or \'skip\' to skip your turn.'), nl, nl,
		read(Input),
		( Input \= skip ->
			write('Please enter a suspect: '), read(Suspect), nl,
			write('Please enter a room: '), read(Room), nl,
			write('Please enter a weapon: '), read(Weapon), nl,
			( Input = a ->
				write('Was the result of our deduction correct? (y/n)'), nl, nl, read(AccusationResult),
				( AccusationResult = y -> win
				; AccusationResult = n -> lose
				)
			; Input = s ->
				write('Please enter a card if you were shown one, otherwise, enter \'none\', example: suspect(mustard)'), nl,
				read(SuggestionResult),
				( SuggestionResult \= none ->
					nextNum(Num, NewNum), gameplay(NewNum)
				; asserta(SuggestionResult), nextNum(Num, NewNum), gameplay(NewNum)
				)
			)
		; nextNum(Num, NewNum), gameplay(NewNum)
		)
	; write('It is Player '), write(Num), write('\'s turn'), nl,
	  write('Has anyone figured it out yet? '), nl,
	  write('Please enter \'a\' for accusation, \'s\' for suggestion or \'skip\' to skip their turn.'), nl,
	  read(Input),
		( Input \= skip ->
			write('Please enter a suspect: '), read(Suspect), nl,
			write('Please enter a room: '), read(Room), nl,
			write('Please enter a weapon: '), read(Weapon), nl,
			( Input = a ->
				write('Was the result of their deduction correct? (y/n)'), nl, read(AccusationResult),
				( AccusationResult = y -> lose
				; AccusationResult = n -> nextNum(Num, NewNum), gameplay(NewNum)
				)
			; Input = s ->
				write('Was a card shown after their suggestion? (y/n)'), nl, read(SuggestionResult),
				( SuggestionResult = y -> nextNum(Num, NewNum), gameplay(NewNum)
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
win :- write('Congratulations, results show that we have conqeured this magestic game of Clue!').

