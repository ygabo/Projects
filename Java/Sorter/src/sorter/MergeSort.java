package sorter;

public class MergeSort {

	MergeSort() {
	}

	public static void sort(Integer[] input) {
		mergesort(input, 0, input.length - 1);
	}

	// private helper
	// this helps with the recursion
	private static void mergesort(Integer[] input, Integer low, Integer high) {

		// get mid since we're splitting
		// array in two
		Integer mid = (high + low) / 2;

		// check if 1 or less elements
		if (high - low < 1)
			return;

		// recurse on left side
		mergesort(input, low, mid);

		// recurse on right side
		mergesort(input, mid + 1, high);

		// now merge
		merge(input, low, mid, mid + 1, high);
	}

	private static void merge(Integer[] input, Integer leftLow,
			Integer leftHigh, Integer rightLow, Integer rightHigh) {

		Integer length = rightHigh - leftLow + 1;
		Integer[] buffer = new Integer[length];
		Integer i = leftLow, j = rightLow, current = 0;

		// fill the buffer in sorted order
		while (i <= leftHigh && j <= rightHigh) {
			buffer[current++] = (input[i] < input[j]) ? input[i++] : input[j++];
		}
		// end of while, means one of two sides have been used up

		// finish up the buffer, since it might have
		// ended prematurely (one side finished before
		// the other so just put them in the buffer
		while (current < length) {
			if (i <= leftHigh)
				buffer[current] = input[i++];
			else
				buffer[current] = input[j++];
			current++;
		}

		// copy back to original array
		current = 0;
		while (current < length) {
			input[leftLow + current] = buffer[current++];
		}
	}
}
