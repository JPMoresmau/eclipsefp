package net.sf.eclipsefp.haskell.browser.views;

import net.sf.eclipsefp.haskell.browser.items.Constructor;
import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.Packaged;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

public class DeclarationsSorter extends ViewerSorter {
	
	@Override
	public int compare(Viewer viewer, Object e1, Object e2) {
		if (e1 instanceof Packaged<?> && e2 instanceof Packaged<?>) {
			Declaration d1 = ((Packaged<Declaration>)e1).getElement();
			Declaration d2 = ((Packaged<Declaration>)e2).getElement();
			return d1.getName().compareToIgnoreCase(d2.getName());
		} else if (e1 instanceof Constructor && e2 instanceof Constructor) {
			Constructor d1 = (Constructor)e1;
			Constructor d2 = (Constructor)e2;
			return d1.getName().compareToIgnoreCase(d2.getName());
		} else {
			return 0;
		}
	}
}
