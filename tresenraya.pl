play :- menu(Board, Difficult), behavior(Board, Difficult), !.

menu(Board, Difficult) :-
  write('You play X.'), nl,
  selectdifficult(Difficult),
  write('Put 0 if you want the AI to start or 1 if you want to start yourself.'),
  read(N),
  firstturn(N, Board, Difficult),
  writeboard([1,2,3,4,5,6,7,8,9]).

selectdifficult(Difficult) :-
  write('Which difficult do you want? [I]mposible, [N]ormal.'), nl,
  write('Write I or N in single quotes.'),
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
  answer([b,b,b,b,b,b,b,b,b], Board, o, Difficult).

firstturn(1, Board, _) :-
   write('You start.'), nl,
   Board = [b,b,b,b,b,b,b,b,b].

firstturn(_, Board, _) :- 
  write('Turn selection is not valid.'),
  read(N),
  firstturn(N,Board,_).

behavior(Board, _) :- win(Board, x), write('You win.').
behavior(Board, _) :- 
  win(Board, o), 
  writeboard(Board),
  write('I win.').
behavior(Board, _) :- 
  not(member(b,Board)), 
  write('Final result.'), nl, nl,
  writeboard(Board),
  write('It\'s a draw.'), nl.
behavior(Board, Difficult) :- 
  writeboard(Board),
  read(N),
  (  move(Board, x, PlayerBoard, N) ;  retrymove(Board, PlayerBoard) ),
  writeboard(PlayerBoard),
  answer(PlayerBoard, AIBoard, o, Difficult), % always answer AI
  behavior(AIBoard, Difficult).

retrymove(Board, PlayerBoard) :-
  write('Illegal move. Please, try again:'), nl,
  read(N),
  (  move(Board, x, PlayerBoard, N) ;  retrymove(Board, PlayerBoard) ).


% AI vs AI game
selfgame :- 
  write('You are in AI vs AI mode.'),
  selectdifficult(Difficult), 
  game([b,b,b,b,b,b,b,b,b],x,Difficult), !.

game(Board, Player,_) :- 
  win(Board, Player), 
  write('Player "'), 
  write(Player), 
  write('" wins.').
game(Board,_,_) :- not(member(b,Board)), write('It\'s a draw.'), nl.
game(Board, Player,Difficult) :-
  turnplayer(Player,Otherplayer),
  answer(Board,NewBoard,Player,Difficult),
  writeboard(NewBoard),
  game(NewBoard,Otherplayer,Difficult).


% Ways to win the game
win(Board, Player) :- Board = [Player,Player,Player,_,_,_,_,_,_].
win(Board, Player) :- Board = [_,_,_,Player,Player,Player,_,_,_].
win(Board, Player) :- Board = [_,_,_,_,_,_,Player,Player,Player].
win(Board, Player) :- Board = [Player,_,_,_,Player,_,_,_,Player].
win(Board, Player) :- Board = [_,_,Player,_,Player,_,Player,_,_].
win(Board, Player) :- Board = [Player,_,_,Player,_,_,Player,_,_].
win(Board, Player) :- Board = [_,Player,_,_,Player,_,_,Player,_].
win(Board, Player) :- Board = [_,_,Player,_,_,Player,_,_,Player].


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

xgame(Board, Player, N) :-
  turnplayer(Player,Oponent),
   (   move(_, Oponent, Board, 1),
   (   move(_, Oponent, Board, 9), N=2 ; move(_, Oponent, Board, 8), N=4 );
   move(_, Oponent, Board, 3),
   (   move(_, Oponent, Board, 7), N=2; move(_, Oponent, Board, 8), N=6 );
    move(_, Oponent, Board, 7),
    move(_, Oponent, Board, 6), N=8).
   

% Answer of AI
answer(Board,PlayerBoard,Player, _) :- 
  move(Board, Player, PlayerBoard,_),
  win(PlayerBoard, Player).
answer(Board,PlayerBoard,Player, _) :-
  opponentwinsnext(Board, Position, Player),
  move(Board, Player, PlayerBoard, Position).
answer(Board,PlayerBoard,Player, 'I') :-
  xgame(Board, Player, N),
  priority2(Board, Player, PlayerBoard, N).
answer(Board,PlayerBoard,Player, 'I') :-
  priority1(Board, Player, PlayerBoard).
answer(Board,PlayerBoard,Player, _) :-
  move(Board, Player, PlayerBoard,_).
answer(Board,PlayerBoard,Player, _) :-
  not(member(b,Board)),
  Board = PlayerBoard,
  Player = Player.

% Differents priorities for impossible mode.
priority1(Board, Player, PlayerBoard):- 
  move(Board, Player, PlayerBoard, 5);
  move(Board, Player, PlayerBoard, 1);
  move(Board, Player, PlayerBoard, 3);
  move(Board, Player, PlayerBoard, 7);
  move(Board, Player, PlayerBoard, 9).

priority2(Board, Player, PlayerBoard, N):- 
  move(Board, Player, PlayerBoard, N).