package sorter;

public class QuickSort {

	QuickSort() {
	}
	
	// public method for quicksort
	public static void sort(Integer[] input) {
		helper(input, 0, input.length - 1);
	}

	// private helper for recursion
	private static void helper(Integer[] input, Integer low, Integer hi) {

		// base case
		// hi - low is less than one when
		// there's only 1 element or less
		if (hi - low < 1)
			return;

		Integer i = low, j = hi, pivot = input[low];
		Integer temp = 0;
		
		// loop until j and i meet up
		while (j > i) {
			// start from left, look for something
			// bigger than pivot
			while (input[i] <= pivot && i < hi) {
				i++;
			}
			// end while, element bigger than pivot found
			
			// from the right, look for something
			// smaller than pivot
			while (input[j] > pivot && j > low)
				j--;
			// end while, something smaller than pivot found
			
			// swap them
			if (i < j) {
				temp = input[i];
				input[i] = input[j];
				input[j] = temp;
			}
			// do it again
		}

		// put where the pivot should be
		// in the array
		// assumed input[low] as the pivot
		// everything to the left of index j
		// is less than or equal to the pivot
		// consequently, everything after the 
		// index j is bigger.
		// so put the pivot in index j (serves as median/middle)
		temp = input[j];
		input[j] = input[low];
		input[low] = temp;
		// index j now has the pivot
		
		// recursively sort left side
		// from lowest index to left neighbor of pivot
		helper(input, low, j-1);
		
		// recursively sort right side
		// starting from the right neighbor
		// of the pivot to the highest index 
		helper(input, j+1, hi);
	}

}
