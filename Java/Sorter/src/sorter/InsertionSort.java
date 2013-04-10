package sorter;

import java.util.ArrayList;
import java.util.Arrays;

public class InsertionSort {

	InsertionSort() {
	};

	// static sort function
	public static void sort(Integer[] unsorted) {
		Integer temp = 0;
		Integer holder = 0, current = 0, other = 0;
		
		// start from the 2nd element, since first element
		// is already "sorted" (list of length 1 is sorted)
		for (int i = 1; i < unsorted.length; i++) {
			temp = i;
			
			// grab the current element
			// swap current element with left neighbor
			// if it is less, do this until we find
			// a left neighbor that is less than the 
			// current element. (we have found the right
			// position of the current element)
			while (temp > 0) {
				
				current = unsorted[temp];
				other = unsorted[temp - 1];
				
				if (current < other) {
					holder = unsorted[temp];
					unsorted[temp] = unsorted[temp - 1];
					unsorted[temp - 1] = holder;
				}
				temp--;
			}
		}
	}

}
