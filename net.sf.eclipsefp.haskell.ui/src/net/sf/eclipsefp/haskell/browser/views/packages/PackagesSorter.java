/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.packages;

import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

/**
 * Sorts packages corresponding to their names and version,
 * in a case-insensitive way.
 * @author Alejandro Serrano
 *
 */
public class PackagesSorter extends ViewerSorter {

	@Override
	public int compare(final Viewer viewer, final Object e1, final Object e2) {
		if (e1 instanceof PackagesItem && e2 instanceof PackagesItem) {
			HaskellPackage p1 = ((PackagesItem)e1).getPackage();
			HaskellPackage p2 = ((PackagesItem)e2).getPackage();
			return p1.getIdentifier().toString().compareToIgnoreCase(p2.getIdentifier().toString());
		} else {
			return 0;
		}
	}
}
