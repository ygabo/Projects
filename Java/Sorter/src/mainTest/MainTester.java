package mainTest;

import sorter.*;
import dataStructures.*;

public class MainTester {

	public static void main(String[] args) {
		Integer[] lol = new Integer[10];

		// insertion sort
		initializeRandom(lol);
		InsertionSort.sort(lol);

		for (int i = 0; i < lol.length; i++) {
			System.out.print(lol[i] + " ");
		}
		
		// insertion sort
		initializeRandom(lol);
		QuickSort.sort(lol);

		for (int i = 0; i < lol.length; i++) {
			System.out.print(lol[i] + " ");
		}

		// insertion sort
		initializeRandom(lol);
		MergeSort.sort(lol);

		for (int i = 0; i < lol.length; i++) {
			System.out.print(lol[i] + " ");
		}
		
		space();
		
		Heap hey = new Heap();
		
		hey.add(1);
		hey.add(2);
		hey.add(52);
		hey.add(3);
		hey.remove();
		hey.add(42);

		hey.remove();
		hey.remove();

		hey.remove();

		hey.remove();
		hey.remove();
		hey.add(52);
		hey.remove();
		hey.add(52);
		hey.add(1);
		hey.add(2);
		hey.add(3);
		//System.out.println(hey.peek());
		hey.printHeap();
	}

	private static void initializeRandom(Integer[] lol) {
		for (int i = 0; i < lol.length; i++) {
			lol[i] = ((int) (Math.random() * 100));
		}
		space();
	}
	
	private static void space(){
		System.out.println();
	}

}
