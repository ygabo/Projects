package sudoku;
import java.util.ArrayList ;
import java.util.HashSet ;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Random ;


public class SudokuSolver {

	/**
	 * @return names of the authors and their IDs (1 per line).
	 */
	public String authors() {
		
		return "Yelnil Gabo 70179064/g3d6" ;
	}

	/**
	 * Performs constraint satisfaction on the given Sudoku board using Arc Consistency and Domain Splitting.
	 * 
	 * @param board the 2d int array representing the Sudoku board. Zeros indicate unfilled cells.
	 * @return the solved Sudoku board
	 */
	public int[][] solve(int[][] board) {
		
		/**
		 *  Figure 1.
		 *  0,  1,  2,    3,  4,  5,   6,  7,  8,
		 *  9,  10, 11,  12, 13, 14,  15, 16, 17, 
		 *  18, 19, 20,  21, 22, 23,  24, 25, 26,
		 *  
		 *  27, 28, 29,  30, 31, 32,  33, 34, 35,
		 *  36, 37, 38,  39, 40, 41,  42, 43, 44,
		 *  45, 46, 47,  48, 49, 50,  51, 52, 53, 
		 *  
		 *  54, 55, 56,  57, 58, 59,  60, 61, 62, 
		 *  63, 64, 65,  66, 67, 68,  69, 70, 71, 
		 *  72, 73, 74,  75, 76, 77,  78, 79, 80,
		 */
		
		int[] sol                = new int[81];
		ArrayList<Integer>[] dom = new ArrayList[81];
		int temp                 = 0 ;
		
		// Convert from 2-d to 1-d with regards to Figure 1.
		for(int x = 0; x < 9; x++) 
		{
			for(int y = 0; y < 9; y++ )
			{
				sol[temp++] = board[x][y];
			}
		}		

		// Initialize arraylist
		for(int i = 0; i < 81; i++)
		{
			dom[ i ] = new ArrayList<Integer>();
		}

		// Fill up each and every variable. (i.e. construct the model
		// for the CSP.) Disregard variables with fixed value, which means
		// it was already given initially. If they have a fixed value, we
		// will fill their corresponding variable with that value. 
		// For example cell 0 will have values { 1,2,3,4,5,6,7,8,9 }, and if
		// cell 1 has been assigned 6 from the input parameter board, it will
		// only have { 6 }.
		// In other words, initialize domains.
		for(int x = 0; x < sol.length; x++ )
		{
			if( sol[ x ] == 0 )
			{
				for( int y = 1; y <= 9; y++ )
				{
					dom[ x ].add( new Integer ( y ) ) ;
				}
			}
			else
			{
				dom[ x ].add( new Integer ( sol[ x ] ) ) ;
			}
		}	

		// do first AC
		doArcConsistentThingy( sol, dom ) ;
		
		// fill board with new answers
		// may not be complete, but still
		for( int x = 0; x < 81 ; x++)
		{
			if( ( sol[ x ] == 0 ) && ( dom[ x ].size() == 1 ) )
			{
				sol[ x ] = dom[ x ].get( 0 ) ;
			}
		}
		
		if( areWeDone( sol ) )
		{
			// done
			temp = 0 ;
			for(int x = 0; x < 9; x++) 
			{
				for(int y = 0; y < 9; y++ )
				{
					board[x][y] = sol[temp++] ;
				}
			}
			return board;
		}
		
		// not done, do AC again with domain splitting
		sol  = ACwithDomSplitting( sol, dom );
		temp = 0 ;
		
		// convert back from 1-d to 2-d with regards to Figure 1
		if( sol != null )
		{
			for(int x = 0; x < 9; x++) 
			{
				for(int y = 0; y < 9; y++ )
				{
					board[x][y] = sol[temp++] ;
				}
			}
		}

		return board;
	}
	
	/**
	 * Heart of the algorithm. Performs Arc consistency with domain splitting.
	 * 
	 * @param sol the 1d int array representing the Sudoku board. Zeros indicate unfilled cells.
	 * @param doms the 2d array list, containing the domains of each cell
	 * @return solved Sudoku board
	 *         null otherwise
	 */
	private int[] ACwithDomSplitting( int[] sol, ArrayList<Integer>[] doms)
	{
		int splitThis                 = 0 ;
		int splitHere                 = 0 ;
		ArrayList<Integer>[] leftdom  = new ArrayList[ 81 ] ;
		ArrayList<Integer>[] rightdom = new ArrayList[ 81 ] ;
		boolean leftOK                = true ;
		boolean rightOK               = true ;
		boolean assigned              = false ;
		int         temp              = 0 ;
		int[] sol2                    = new int[ 81 ] ;
		int[] sol1                    = new int[ 81 ] ;
		
		
		// Initialize arrays and lists
		for(int i = 0; i < 81; i++)
		{
			sol1[ i ]     = sol[ i ];
			sol2[ i ]     = sol[ i ];
			leftdom[ i ]  = new ArrayList<Integer>();
			rightdom[ i ] = new ArrayList<Integer>();
		}
		
		for(int i = 0; i < 81; i++)
		{
			copyArrayList( leftdom[ i ] , doms[ i ] ) ;
			copyArrayList( rightdom[ i ], doms[ i ] ) ;
		}
		
		// pick domain to split
		for( int x = 0; x < 81; x++ )
		{
			if( doms[ x ].size() > 1 )
			{
				splitThis = x ;
				assigned = true;
			}
			else if( doms[ x ].size() == 0 ) 
				return null ;
		}
		
		// if none picked, all domains == 1
		// aka have 1 possible answer per box
		// do AC to check if they're correct
		// if not return null, if they are
		// return the board
		if( !assigned )
		{
			doArcConsistentThingy( sol, doms ) ;
			
			for( int x = 0; x < 81 ; x++)
			{
				if( doms[x].size() == 0 )
				{
					return null ;
				}
			}
			
			for( int x = 0; x < 81 ; x++)
			{
				if( ( sol[ x ] == 0 ) && ( doms[ x ].size() == 1 ) )
				{
					sol[ x ] = doms[ x ].get( 0 ) ;
				}
			}
			
			return sol ;
		}
		
		// split domain
		// get middle.
		splitHere = doms[ splitThis ].size() / 2  ;
		temp      = splitHere ;
		
		// leftdom, as in left domain, has first half
		while ( leftdom[ splitThis ].size() > splitHere )
		{
			leftdom[ splitThis ].remove( leftdom[ splitThis ].size() - 1 ) ;
		}

		// rightdom, as in right domain, has second half
		while ( temp > 0)
		{
			rightdom[ splitThis ].remove( 0 ) ;
			temp-- ;
		}

		// do AC on left domain
		doArcConsistentThingy( sol1, leftdom ) ;

		// check if any domain is length 0 -- i.e. any domain that is empty
		for( int x = 0; x < 81 ; x++)
		{
			if( leftdom[x].isEmpty() )
			{
				leftOK = false ;
			}
		}
		
		// fill board 1 with new answers
		// may not be complete
		for( int x = 0; x < 81 ; x++)
		{
			if( ( sol1[ x ] == 0 ) && ( leftdom[ x ].size() == 1 ) )
			{
				sol1[ x ] = leftdom[ x ].get( 0 ) ;
			}
		}
		
		// do AC on right domain
		doArcConsistentThingy( sol2, rightdom ) ;

		// check if any domain is length 0 -- i.e. any domain that is empty
		for( int x = 0; x < 81 ; x++)
		{
			if( rightdom[x].isEmpty() )
			{
				rightOK = false ;
			}
		}
		
		// fill board 2 with new answers
		// may not be complete
		for( int x = 0; x < 81 ; x++)
		{
			if( ( sol2[ x ] == 0 ) && ( rightdom[ x ].size() == 1 ) )
			{
				sol2[ x ] = rightdom[ x ].get( 0 ) ;
			}
		}
		
		// recursion, repeat whole thing with split domains
		if( leftOK )
		{
			sol1 = ACwithDomSplitting( sol1, leftdom ) ;
		}
		else
		{
			sol1 = null ;
		}
		
		if( rightOK )
		{
			sol2 = ACwithDomSplitting( sol2, rightdom ) ;
		}
		else
		{
			sol2 = null ;
		}

		// return proper solutions
		if( areWeDone( sol1 ) )
		{
			return sol1 ;
		}
		
		if( areWeDone( sol2 ) )
		{
			return sol2 ;
		}

		// return null, no solutions found on this path
		return null ;
		
	}

	/**
	 * Arc consistent algorithm
	 * 
	 * @param sol the 1d int array representing the Sudoku board. Zeros indicate unfilled cells.
	 * @params doms the 2d arraylist representing the domains of each cell in the board
	 * @return arc consistent sudoku sol
	 */
	private void doArcConsistentThingy( int[] sol, ArrayList<Integer>[] doms )
	{
		LinkedList<Arcs> TDA = new LinkedList<Arcs>() ;
		ArrayList<Integer> consistent ;
		int curr_cell, other_cell ;
		Iterator<Integer> itr1 ;
		Iterator<Integer> itr2 ;
		Integer tempOther ;
		Arcs tempArc ;
		Integer temp ;

		// fill up to-do list
		for ( int i = 0; i < 81; i ++ )
		{
			fillTDA( TDA, i ) ;
		}

		// if there is an Arc to do, loop through all
		// while its not empty
		while( TDA.size() > 0 )
		{
			
			tempArc    = TDA.remove() ; // get one 
			curr_cell  = tempArc.curr_cell ; // get info out of Arc
			other_cell = tempArc.other_cell ;
			consistent = new ArrayList<Integer>() ; // list of consistent domain values
			itr1 = doms[ curr_cell ].iterator() ;

			// loop through all the current cell's values
			while( itr1.hasNext() )
			{				
				temp = itr1.next() ;
				itr2 = doms[ other_cell ].iterator() ;
				
				// if other cell is empty
				// this solution is invalid already, but still add
				// the current cell's value to consistent
				if ( doms[ other_cell ].isEmpty() )
				{
					consistent.add( temp ) ;
				}

				// while there is still value for other cell
				while( true )
				{
					if( itr2.hasNext() )
					{
						tempOther = itr2.next() ;
						// if constraint achieved
						if( !temp.equals( tempOther ))
						{
							// add to consistent pile, then break
							consistent.add( temp ) ;
							break ;
						}
					}
					else
					{
						break ;
					}
				}
			}
			
			if( consistent.size() != doms[ curr_cell ].size() )
			{
				// meaning something got removed in the domain
				// re-add everything thats dependent to current cell
				// to check for AC again
				reAddToTDA( TDA, curr_cell ) ;
				// set the new domain
				copyArrayList( doms[ curr_cell ], consistent ) ;
			}
		}	    
	}
	
	/**
	 *  fill up the TDA
	 * 
	 * @param TDA a linked list of all arcs representing arcs we have to check
	 * @params x is the position of the cell to be added in the TDA
	 * @return TDA with all arcs of all the cells
	 */
	private void fillTDA( LinkedList<Arcs> TDA, int x)
	{
		// get row and column of position x
		int r = getRow( x ) ;
		int c = getCol( x ) ;
		
		for ( int y = 0; y < 9 ; y++ )
		{
			// add all other cells in the same row as x
			if ( y != r )
			{
				TDA.add( new Arcs( x, getPosition( c, y ) ) ) ;
			}
			
			// add all other cells in the same column x
			if ( y != c )
			{
				TDA.add( new Arcs( x, getPosition( y, r ) ) ) ;
			}
		}
		
		int col_block = c / 3;
		int row_block = r / 3;
		
		// this represents adding all the arcs from x to all other
		// cells inside the block where x is
		for ( int m = col_block * 3 ; m < col_block * 3 + 3 ; ++m )
		{
			for ( int n = row_block * 3 ; n < row_block * 3 + 3 ; ++n )
			{
				// if not the same coordinate, add to TDA
				if ( m != c && n != r )
				{
					TDA.add(new Arcs( x, getPosition( m, n ) ) ) ;
				}
			}
		}		
	}
	
	/**
	 *  refill the TDA
	 * 
	 * @param TDA a linked list of all arcs representing arcs we have to check
	 * @params x is the position of the cell to be added in the TDA
	 * @return TDA with additional arcs of all the cells that were affected by x
	 */
	private void reAddToTDA( LinkedList<Arcs> TDA, int x)
	{
		int r = getRow( x ) ;
		int c = getCol( x ) ;
		
		for ( int y = 0; y < 9 ; y++ )
		{
			// add all other cells in the row
			if ( y != r )
			{
				TDA.add( new Arcs( getPosition( c, y ), x ) ) ;
			}
			// add all other cells in the column
			if ( y != c )
			{
				TDA.add( new Arcs( getPosition( y, r ), x ) ) ;
			}
		}

		int col_block = c / 3;
		int row_block = r / 3;
		
		// this represents adding all the arcs from all other
		// cells to x, inside the block where x is
		for ( int m = col_block * 3 ; m < col_block * 3 + 3 ; ++m )
		{
			for ( int n = row_block * 3 ; n < row_block * 3 + 3 ; ++n )
			{
				// if not the same coordinate, add to TDA
				if ( m != c && n != r )
				{
					TDA.add(new Arcs( getPosition( m, n ), x ) ) ;
				}
			}
		}
	}
	
	/**
	 *  get the row of position x
	 * 
	 * @params x is the position of the cell 
	 * @return row where x is
	 */
	private int getRow( int x )
	{
		return ( int ) Math.floor( x / 9 ) ;
	}

	/**
	 *  get the column of position x
	 * 
	 * @params x is the position of the cell 
	 * @return column where x is
	 */
	private int getCol( int x )
	{
		return x % 9 ;
	}	
	
	/**
	 *  get position of the coordinate (x, y)
	 * 
	 * @params x is the row
	 * @params y is the column
	 * @return position of (x, y) according to figure 2
	 */
	private int getPosition(int x, int y)
	{
		/**
		 *  Figure 2.
		 *     0   1   2     3   4   5    6   7   8
		 *     
		 *  0  0,  1,  2,    3,  4,  5,   6,  7,  8,
		 *  1  9,  10, 11,  12, 13, 14,  15, 16, 17, 
		 *  2  18, 19, 20,  21, 22, 23,  24, 25, 26,
		 *  
		 *  3  27, 28, 29,  30, 31, 32,  33, 34, 35,
		 *  4  36, 37, 38,  39, 40, 41,  42, 43, 44,
		 *  5  45, 46, 47,  48, 49, 50,  51, 52, 53, 
		 *  
		 *  6  54, 55, 56,  57, 58, 59,  60, 61, 62, 
		 *  7  63, 64, 65,  66, 67, 68,  69, 70, 71, 
		 *  8  72, 73, 74,  75, 76, 77,  78, 79, 80,
		 */
		int pos = 0 ;
		
		for( int t = 0; t < 9; t++ )
		{
			for( int r = 0; r < 9; r ++ )
			{
				if ( y == t && x == r )
				{
					return pos ;
				}
				else
				{
					pos++ ;
				}					
			}
		}
		
		return pos ;
	}
	
	/**
	 *  get position of the coordinate (x, y)
	 * 
	 * @params x is the row
	 * @params y is the column
	 * @return position of (x, y) according to figure 2
	 */
	private void copyArrayList( ArrayList<Integer> oldDom, ArrayList<Integer> newDom )
	{
		Iterator<Integer> itr = newDom.iterator() ;
		Integer x             = 0 ;
		
		oldDom.clear() ;
		
		while( itr.hasNext() )
		{
			x = itr.next() ;
			oldDom.add( x ) ;			
		}		
	}
	
	/**
	 *  check if board is done
	 * 
	 * @params @param sol the 1d int array representing the Sudoku board. Zeros indicate unfilled cells.
	 * @params 
	 * @return true if all values in sol are non zeroes, false otherwise
	 */
	private boolean areWeDone( int[] sol )
	{
		if( sol == null )
		{
			return false ;
		}
		
		for( int x = 0; x < sol.length; x++ )
		{
			if( sol[ x ] == 0 )
			{
				return false ;
			}
		}
		
		return true ;
	}
	
	/**
	 *  Class Arcs.
	 *  Representing the link from one cell to another
	 * 
	 */
	public class Arcs
	{
		int curr_cell ;
		int other_cell ;
		
		public Arcs(int a, int b)
		{
			curr_cell = a;
			other_cell = b;
		}
	}	
}
