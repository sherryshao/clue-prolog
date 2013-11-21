% 2013W1 CPSC 312 101
% Project #2
% Eric Chu (73265092) & Sherry Shao (60135084)

start :- 
	write('Who is your character?'), nl,
	read(Player), asserta(iam(Player)),
	write('You are now set as '), write(Player), nl,
	setup.

setup :- 
	write('Enter a card type or type \'done\' if you\'ve entered all your cards: (ex. suspect)'), nl,
	read(Type),
	( Type \= done ->  
		write('Enter its name: (ex. mustard)'), nl, read(Card),
		( Type = suspect -> asserta(suspect(Card)), setup
		; Type = weapon -> asserta(weapon(Card)), setup
		; Type = room -> asserta(room(Card)), setup
		)
	;	next
	).

next.

