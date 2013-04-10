package dataStructures;

import java.util.ArrayList;

public class Heap {

	ArrayList<Integer> heap;
	Integer size;
	
	// This will be a max-heap
	// max element is at top of heap
	public Heap() {
		this.heap = new ArrayList<Integer>();
		this.heap.add(0);
		size = 0;
	}

	public void add(Integer num) {

		this.size++;
		
		// attach 0 to the end to increase size of heap
		// if we need to
		while( heap.size() < this.size + 1 ) this.heap.add(0);
		// currIndex has 0 for now
		Integer currIndex = this.size;

		// figure out if added number should
		// be 'promoted'
		Integer parentIndex = getParent(currIndex);

		// keep looping if num is bigger than the parents
		// stop when num is small than parent, or reached
		// the top
		while (heap.get(parentIndex) < num && parentIndex >= 1) {

			// demote parent
			heap.set(currIndex, heap.get(parentIndex));

			// our new place
			currIndex = parentIndex;
			// get new parent index
			parentIndex = getParent(currIndex);
			// now do it again, and see if ancestor is still smaller
			// than our elment
		}
		// end while loop, either we're at top
		// or this current index' has a parent with bigger value
		
		// currIndex is the correct place for number 
		// in the heap
		heap.set(currIndex, num);
	}

	// removes the max element
	// fix heap afterwards before returning max
	public Integer remove() {
		if (heap.size() < 2)
			return null;
		
		Integer toReturn = this.heap.get(1), current = 1;
		Integer leftIndex = 0, rightIndex = 0;

		// we have to promote the bigger children
		while (hasChildren(current)) {
			leftIndex = getLeftChildIndex(current);
			rightIndex = getRightChildIndex(current);
			
			if (heap.get(leftIndex) >= heap.get(rightIndex) || rightIndex == 0) {
				heap.set(current, heap.get(leftIndex));
				current = leftIndex;
			} else if (heap.get(rightIndex) > heap.get(leftIndex)
					|| leftIndex == 0) {
				heap.set(current, heap.get(rightIndex));
				current = rightIndex;
			}
		}
		// end of while, means we got to the bottom of heap

		// remove last element since it is now not needed
		heap.remove( (int) current);
		this.size--;
		return toReturn;
	}

	public Integer peek() {
		return heap.get(1);
	}
	
	public Integer getSize()
	{
		return this.size;
	}
	
	public void printHeap()
	{
		for (int i = 0; i < heap.size(); i ++ )
		{
			print(heap.get(i));
		}
	}
	
	private Integer getParent(Integer index) {
		Integer parent = index / 2;
		return (parent >= 1) ? parent : 0;
	}

	private Integer getLeftChildIndex(Integer index) {
		Integer child = index * 2;
		return (child < heap.size()) ? child : 0;
	}

	private Integer getRightChildIndex(Integer index) {
		Integer child = (index * 2) + 1;
		return (child < heap.size()) ? child : 0;
	}

	private boolean hasChildren(Integer index) {
		return getLeftChildIndex(index) != 0 ;
	}
	
	private void debug()
	{
		System.out.println("---");
	}
	
	private void print(Object lol)
	{
		System.out.println(lol.toString());
	}
}
