package net.sf.eclipsefp.haskell.scion.internal.util;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * A multiset, or bag, which is like a set but can contain the same value more than once.
 * 
 * @author Thomas ten Cate
 *
 * @param <T> the type of values this multiset contains
 */
public class Multiset<T> {

	private Map<T, Integer> counts = new HashMap<T, Integer>();
	
	public Multiset() {
	}
	
	/**
	 * Checks whether an element is contained in this multiset. 
	 * 
	 * @return whether this multiset contains at least one instance of the element
	 */
	public boolean contains(T element) {
		return counts.containsKey(element);
	}
	
	/**
	 * Counts the number of times the element is contained in this multiset.
	 * 
	 * @return the number of occurrences of the element
	 */
	public int count(T element) {
		if (contains(element))
			return counts.get(element);
		else
			return 0;
	}
	
	/**
	 * Adds an element to the multiset.
	 */
	public void add(T element) {
		int count = count(element);
		++count;
		counts.put(element, Integer.valueOf(count));
	}
	
	/**
	 * Removes an element from the multiset.
	 * If it does not contain the element, nothing is changed.
	 */
	public void remove(T element) {
		int count = count(element);
		if (count > 0) {
			--count;
			if (count > 0) {
				counts.put(element, Integer.valueOf(count));
			} else {
				counts.remove(count);
			}
		}
	}
	
	/**
	 * Removes all elements from the multiset.
	 */
	public void clear() {
		counts.clear();
	}
	
	/**
	 * Returns a set that contains all values from this multiset exactly once.
	 */
	public Set<T> uniqueSet() {
		return counts.keySet();
	}

}
