:- include('KB.pl').

cell(X,Y):-
	grid(X1,Y1),
	X >= 0,
	X < X1,
	Y >= 0,
	Y < Y1.
	
state(LocX, LocY, C, Ships, s0):-
	agent_loc(LocX, LocY),
	capacity(C),
	Ships=[].

state(LocX, LocY, C, Ships_loc, result(A,S)):-
        cell(LocX,LocY),(
        (A=pickup, C1 is C+1, not(C1==0),ships_loc(AllShips), member([LocX,LocY], AllShips), delete(Ships_loc, [LocX,LocY], Ships),state(LocX, LocY, C1, Ships, S));
        (A=drop, capacity(Max), C is Max, ((Max==1, C1 is 0); (Max==2,(C1 is C-1;C1 is 0))),C1<Max , state(LocX,LocY,C1,Ships_loc,S), station(LocX,LocY));
        ( A = left, Y2 is LocY+1 ,state(LocX, Y2,C,Ships_loc,S));
        ( A = right,  Y2 is LocY-1,state(LocX, Y2,C,Ships_loc,S));
        ( A = up, X2 is LocX+1, state(X2, LocY,C,Ships_loc,S));
        ( A = down, X2 is LocX-1, state(X2, LocY,C,Ships_loc,S))).
        


goal2(S):-  
    ships_loc(Ships), capacity(C), station(LocX,LocY),state(LocX,LocY,C,Ships,S).

ids(X,L):-
    (call_with_depth_limit(goal2(X),L,R), number(R));
    (call_with_depth_limit(goal2(X),L,R), R=depth_limit_exceeded, L1 is L+1, ids(X,L1)).

goal(S):-
        ids(S,1).
