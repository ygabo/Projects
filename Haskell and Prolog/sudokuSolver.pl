% Yelnil Gabo 70179064 g3d6 yelnil@gmail.com

% This sudoku solver can only solve the basic tests(0a,0b,0c)

/* This runs all the simple tests. If it 
works correctly, you should see three identical 
and completed suduko tables, and finally the 
word No (as test0c will fail.) */
test :-
	test0, nl,
	test0a, nl,
	test0b, nl,
	test0c.

/* This is a completly solved solution. */
test0 :-
	L = [
             [9,6,3,1,7,4,2,5,8],
             [1,7,8,3,2,5,6,4,9],
             [2,5,4,6,8,9,7,3,1],
             [8,2,1,4,3,7,5,9,6],
             [4,9,6,8,5,2,3,1,7],
             [7,3,5,9,6,1,8,2,4],
             [5,8,9,7,1,3,4,6,2],
             [3,1,7,2,4,6,9,8,5],
             [6,4,2,5,9,8,1,7,3]],
        sudoku(L),
        printsudoku(L).

/* This has a solution (the one in test0) which 
should be found very quickly. */
test0a :-
	L = [
             [9,_,3,1,7,4,2,5,8],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,8,9,7,3,1],
             [8,2,1,4,3,7,5,_,6],
	     [4,9,6,8,5,2,3,1,7],
             [7,3,_,9,6,1,8,2,4],
             [5,8,9,7,1,3,4,6,2],
             [3,1,7,2,4,6,9,8,5],
             [6,4,2,5,9,8,1,7,3]],
        sudoku(L),
        printsudoku(L).

/* This has a solution (the one in test0) and 
may take a few seconds to find. */
test0b :-
	L = [
             [9,_,3,1,7,4,2,5,_],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,_,9,_,3,1],
             [_,2,1,4,3,_,5,_,6],
             [4,9,_,8,_,2,3,1,_],
             [_,3,_,9,6,_,8,2,_],
             [5,8,9,7,1,3,4,6,2],
             [_,1,7,2,_,6,_,8,5],
             [6,4,2,5,9,8,1,7,3]],
        sudoku(L),
        printsudoku(L).

/* This one obviously has no solution (column 2 has 
two nines in it.) and it may take a few seconds 
to deduce this. */
test0c :-
	L = [
             [_,9,3,1,7,4,2,5,8],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,8,9,7,3,1],
             [8,2,1,4,3,7,5,_,6],
	     [4,9,6,8,5,2,3,1,7],
             [7,3,_,9,6,1,8,2,4],
             [5,8,9,7,1,3,4,6,2],
             [3,1,7,2,4,6,9,8,5],
             [6,4,2,5,9,8,1,7,3]],
        sudoku(L),
        printsudoku(L).

/* Here is an extra test for you to try. It would be
nice if your program can solve this puzzle, but it's
not a requirement. */

test0d :-
	L = [
             [9,_,3,1,_,4,2,5,_],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,_,9,_,3,1],
             [_,2,1,4,3,_,5,_,6],
             [4,9,_,8,_,2,3,1,_],
             [_,3,_,9,6,_,8,2,_],
             [5,8,9,7,1,3,4,6,2],
             [_,1,7,2,_,6,_,8,5],
             [6,4,2,5,_,8,1,7,3]],
        sudoku(L),
        printsudoku(L).


/* The next 3 tests are supposed to be progressively 
harder to solve. Our first attempt at a solver did not 
find a solution in a reasonable length of time for 
any of these, so if you manage to write a solver 
that does them in a reasonable length of time, 
expect to recieve top or possibly bonus marks. (BUT 
YOU MUST TELL US THIS IN YOUR EMAIL OR WE WON'T RUN 
THESE TESTS.) */
test1 :-
	L = [
             [_,6,_,1,_,4,_,5,_],
             [_,_,8,3,_,5,6,_,_],
             [2,_,_,_,_,_,_,_,1],
             [8,_,_,4,_,7,_,_,6],
	         [_,_,6,_,_,_,3,_,_],
             [7,_,_,9,_,1,_,_,4],
             [5,_,_,_,_,_,_,_,2],
             [_,_,7,2,_,6,9,_,_],
             [_,4,_,5,_,8,_,7,_]],
        sudoku(L),
        printsudoku(L).

test2 :-
	L = [
             [_,_,4,_,_,3,_,7,_],
             [_,8,_,_,7,_,_,_,_],
             [_,7,_,_,_,8,2,_,5],
             [4,_,_,_,_,_,3,1,_],
	         [9,_,_,_,_,_,_,_,8],
             [_,1,5,_,_,_,_,_,4],
             [1,_,6,9,_,_,_,3,_],
             [_,_,_,_,2,_,_,6,_],
             [_,2,_,4,_,_,5,_,_]],
        sudoku(L),
        printsudoku(L).

test3 :-
	L = [
             [_,4,3,_,8,_,2,5,_],
	         [6,_,_,_,_,_,_,_,_],
             [_,_,_,_,_,1,_,9,4],
             [9,_,_,_,_,4,_,7,_],
             [_,_,_,6,_,8,_,_,_],
             [_,1,_,2,_,_,_,_,3],
             [8,2,_,5,_,_,_,_,_],
             [_,_,_,_,_,_,_,_,5],
             [_,3,4,_,9,_,7,1,_]],
        sudoku(L),
        printsudoku(L).


% print suduko table
printsudoku([]).
printsudoku([H|T]) :-
	write(H),nl,
	printsudoku(T).

% Expects a list of lists 9 by 9 grid.
sudoku(L) :- L = [[A1,A2,A3, A4,A5,A6, A7,A8,A9],
				  [B1,B2,B3, B4,B5,B6, B7,B8,B9],
				  [C1,C2,C3, C4,C5,C6, C7,C8,C9],

				  [D1,D2,D3, D4,D5,D6, D7,D8,D9],
				  [E1,E2,E3, E4,E5,E6, E7,E8,E9],
				  [F1,F2,F3, F4,F5,F6, F7,F8,F9],
					
				  [G1,G2,G3, G4,G5,G6, G7,G8,G9],
				  [H1,H2,H3, H4,H5,H6, H7,H8,H9],
				  [J1,J2,J3, J4,J5,J6, J7,J8,J9]], 
				% wops forgot my alphabet... too lazy to change this back
				
                            % once a row has been bounded, check right away if it doesnt have duplicates
                            % that improves performance so it disregards alot of redundancies.
 
			    % check row if valid
				valid(A1), valid(A2), valid(A3), valid(A4), valid(A5),
				valid(A6), valid(A7), valid(A8), valid(A9),
 			    % check right away if theyre different values	
				diff([A1,A2,A3,A4,A6,A7,A8,A9]),
			    % check row if valid	
				valid(B1),
				valid(B2), valid(B3), valid(B4), valid(B5), valid(B6),
				valid(B7), valid(B8), valid(B9),
				diff([B1,B2,B3,B4,B5,B6,B7,B8,B9]),

			    % check row if valid	
				valid(C1), valid(C2),
				valid(C3), valid(C4), valid(C5), valid(C6), valid(C7),
				valid(C8), valid(C9), 				
				diff([C1,C2,C3,C4,C5,C6,C7,C8,C9]),
				
				diff([A1,A2,A3,B1,B2,B3,C1,C2,C3]),
				diff([A4,A5,A6,B4,B5,B6,C4,C5,C6]),
				diff([A7,A8,A9,B7,B8,B9,C7,C8,C9]),

			    % check row if valid	
				valid(D1), valid(D2), valid(D3),
				valid(D4), valid(D5), valid(D6), valid(D7), valid(D8),
				valid(D9), 
				diff([D1,D2,D3,D4,D5,D6,D7,D8,D9]),
			 % check row if valid	
				valid(E1), valid(E2), valid(E3), valid(E4),
				valid(E5), valid(E6), valid(E7), valid(E8), valid(E9),
				diff([E1,E2,E3,E4,E5,E6,E7,E8,E9]),
				
				valid(F1), valid(F2), valid(F3), valid(F4), valid(F5),
				valid(F6), valid(F7), valid(F8), valid(F9), 
				diff([F1,F2,F3,F4,F5,F6,F7,F8,F9]),
				
				diff([D1,D2,D3,E1,E2,E3,F1,F2,F3]),
				diff([D4,D5,D6,E4,E5,E6,F4,F5,F6]),
				diff([D7,D8,D9,E7,E8,E9,F7,F8,F9]),
				
			 % check row if valid	
				valid(G1),
				valid(G2), valid(G3), valid(G4), valid(G5), valid(G6),
				valid(G7), valid(G8), valid(G9), 
				diff([G1,G2,G3,G4,G5,G6,G7,G8,G9]),
				valid(H1), valid(H2),
				valid(H3), valid(H4), valid(H5), valid(H6), valid(H7),
				valid(H8), valid(H9),
				diff([H1,H2,H3,H4,H5,H6,H7,H8,H9]),
			 % check row if valid	
				valid(J1), valid(J2), valid(J3),
				valid(J4), valid(J5), valid(J6), valid(J7), valid(J8),
				valid(J9),
				diff([J1,J2,J3,J4,J5,J6,J7,J8,J9]),				
								
				diff([G1,G2,G3,H1,H2,H3,J1,J2,J3]),
				diff([G4,G5,G6,H4,H5,H6,J4,J5,J6]),
				diff([G7,G8,G9,H7,H8,H9,J7,J8,J9]),
												

				% CHECK COLUMNS
				diff([A1,B1,C1,D1,E1,F1,G1,H1,J1]),
				diff([A2,B2,C2,D2,E2,F2,G2,H2,J2]),
				diff([A3,B3,C3,D3,E3,F3,G3,H3,J3]),
				diff([A4,B4,C4,D4,E4,F4,G4,H4,J4]),
				diff([A5,B5,C5,D5,E5,F5,G5,H5,J5]),
				diff([A6,B6,C6,D6,E6,F6,G6,H6,J6]),
				diff([A7,B7,C7,D7,E7,F7,G7,H7,J7]),
				diff([A8,B8,C8,D8,E8,F8,G8,H8,J8]),
				diff([A9,B9,C9,D9,E9,F9,G9,H9,J9]).
				
% helper that returns true if all elements in list is unique				
diff([]).
diff([H|T]) :- not(member(H,T)),diff(T). 

% helper that returns true if X is a number from 1-9
valid(X) :- member(X, [1,2,3,4,5,6,7,8,9]).
