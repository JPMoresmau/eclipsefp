package net.sf.eclipsefp.haskell.browser.views;

import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

public class PackagesSorter extends ViewerSorter {

	@Override
	public int compare(Viewer viewer, Object e1, Object e2) {
		if (e1 instanceof PackagesItem && e2 instanceof PackagesItem) {
			HaskellPackage p1 = ((PackagesItem)e1).getPackage();
			HaskellPackage p2 = ((PackagesItem)e2).getPackage();
			return p1.getIdentifier().toString().compareToIgnoreCase(p2.getIdentifier().toString());
		} else {
			return 0;
		}
	}
}
