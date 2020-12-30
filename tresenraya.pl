play :- menu(Board, Difficult), behavior(Board, Difficult).

menu(Board, Difficult) :-
  write('You play X by entering integer positions followed by a period.'), nl,
  selectdifficult(Difficult),
  writeboard([1,2,3,4,5,6,7,8,9]),
  write('Put 0 if you want the AI to start or 1 if you want to start yourself.'),
  read(N),
  firstturn(N, Board, Difficult).

selectdifficult(Difficult) :-
  write('Which difficult do you want? [I]mposible, [N]ormal.'),
  read(Difficult),  % Put in single quotes
  difficult(Difficult).

difficult('I') :- write('Now you are in impossible mode.'), nl, nl.
difficult('N') :- write('Now you are in normal mode.'), nl, nl.
difficult(_) :-
  write('Difficult selection is not valid.'), nl, 
  read(D),
  difficult(D).

turnplayer(x,o).
turnplayer(o,x).

firstturn(0, Board, Difficult) :-
  write('AI start.'), nl,
  respond([b,b,b,b,b,b,b,b,b], Board,o,Difficult),
  writeboard(Board).

firstturn(1, Board, _) :-
   write('You start.'), nl,
   Board = [b,b,b,b,b,b,b,b,b].

firstturn(_,Board, _) :- 
  write('Turn selection is not valid.'),
  read(N),
  firstturn(N,Board,_).

behavior(Board, _) :- win(Board, x), write('You win!').
behavior(Board, _) :- win(Board, o), write('I win!').
behavior(Board, _) :- not(member(b,Board)), write('It\'s a draw!'), nl.
behavior(Board, Difficult) :- 
  writeboard(Board),
  read(N),
  (  move(Board, x, PlayerBoard, N) ;  retrymove(Board, PlayerBoard) ),
  writeboard(PlayerBoard),
  respond(PlayerBoard, IABoard, o, Difficult), % always respond IA
  behavior(IABoard, Difficult).

retrymove(Board, PlayerBoard) :-
  write('Illegal move. Please, try again:'), nl,
  read(N),
  (  move(Board, x, PlayerBoard, N) ;  retrymove(Board, PlayerBoard) ).


% AI vs AI game
selfgame :- 
  write('You are in AI vs AI mode.'),
  selectdifficult(Difficult), 
  game([b,b,b,b,b,b,b,b,b],x,Difficult).

game(Board, Player,_) :- win(Board, Player), !, write([player, Player, wins]).
game(Board,_,_) :- not(member(b,Board)), write('It\'s a draw!'), nl.
game(Board, Player,Difficult) :-
  turnplayer(Player,Otherplayer),
  respond(Board,NewBoard,Player,Difficult),
  writeboard(NewBoard),
  game(NewBoard,Otherplayer,Difficult).


% Ways to win the game
win(Board, Player) :- rowwin(Board, Player).
win(Board, Player) :- colwin(Board, Player).
win(Board, Player) :- diagwin(Board, Player).

rowwin(Board, Player) :- Board = [Player,Player,Player,_,_,_,_,_,_].
rowwin(Board, Player) :- Board = [_,_,_,Player,Player,Player,_,_,_].
rowwin(Board, Player) :- Board = [_,_,_,_,_,_,Player,Player,Player].

colwin(Board, Player) :- Board = [Player,_,_,Player,_,_,Player,_,_].
colwin(Board, Player) :- Board = [_,Player,_,_,Player,_,_,Player,_].
colwin(Board, Player) :- Board = [_,_,Player,_,_,Player,_,_,Player].

diagwin(Board, Player) :- Board = [Player,_,_,_,Player,_,_,_,Player].
diagwin(Board, Player) :- Board = [_,_,Player,_,Player,_,Player,_,_].


% Movements in the board
move([b,B,C,D,E,F,G,H,I], Player, [Player,B,C,D,E,F,G,H,I], 1).
move([A,b,C,D,E,F,G,H,I], Player, [A,Player,C,D,E,F,G,H,I], 2).
move([A,B,b,D,E,F,G,H,I], Player, [A,B,Player,D,E,F,G,H,I], 3).
move([A,B,C,b,E,F,G,H,I], Player, [A,B,C,Player,E,F,G,H,I], 4).
move([A,B,C,D,b,F,G,H,I], Player, [A,B,C,D,Player,F,G,H,I], 5).
move([A,B,C,D,E,b,G,H,I], Player, [A,B,C,D,E,Player,G,H,I], 6).
move([A,B,C,D,E,F,b,H,I], Player, [A,B,C,D,E,F,Player,H,I], 7).
move([A,B,C,D,E,F,G,b,I], Player, [A,B,C,D,E,F,G,Player,I], 8).
move([A,B,C,D,E,F,G,H,b], Player, [A,B,C,D,E,F,G,H,Player], 9).

writeboard([A,B,C,D,E,F,G,H,I]) :- 
  write([A,B,C]),nl,
  write([D,E,F]),nl,
  write([G,H,I]),nl,nl.

opponentwinsnext(Board, Position, Player) :- 
  turnplayer(Player, Oponent),
  move(Board, Oponent, PlayerBoard, Position),
  win(PlayerBoard, Oponent).

xgame(Board, Player) :-
  turnplayer(Player,Oponent),
  (   move(Board, Player, PlayerBoard, 1),
      move(Board, Oponent, PlayerBoard, 9);
      move(Board, Oponent, PlayerBoard, 3),
      move(Board, Oponent, PlayerBoard, 7)).

% Answer of AI
respond(Board,PlayerBoard,Player, _) :- 
  move(Board, Player, PlayerBoard,_),
  win(PlayerBoard, Player), !.
respond(Board,PlayerBoard,Player, _) :-
  opponentwinsnext(Board, Position, Player),
  move(Board, Player, PlayerBoard, Position).
respond(Board,PlayerBoard,Player, 'I') :-
  xgame(Board, Player),
  priority2(Board, Player, PlayerBoard, N),	
  move(Board, Player, PlayerBoard,N).
respond(Board,PlayerBoard,Player, 'I') :-
  priority1(Board, Player, PlayerBoard, N),
  move(Board, Player, PlayerBoard,N).
respond(Board,PlayerBoard,Player, _) :-
  move(Board, Player, PlayerBoard,_).
respond(Board,PlayerBoard,Player, _) :-
  not(member(b,Board)),
  Board = PlayerBoard,
  Player = Player.

% Differents priorities for impossible mode.
priority1(Board, Player, PlayerBoard, N):- 
  move(Board, Player, PlayerBoard, 5), N=5;
  move(Board, Player, PlayerBoard, 1), N=1;
  move(Board, Player, PlayerBoard, 3), N=3;
  move(Board, Player, PlayerBoard, 7), N=7;
  move(Board, Player, PlayerBoard, 9), N=9.

priority2(Board, Player, PlayerBoard, N):- 
  move(Board, Player, PlayerBoard, 2), N=2;
  move(Board, Player, PlayerBoard, 4), N=4;
  move(Board, Player, PlayerBoard, 6), N=6;
  move(Board, Player, PlayerBoard, 8), N=8.