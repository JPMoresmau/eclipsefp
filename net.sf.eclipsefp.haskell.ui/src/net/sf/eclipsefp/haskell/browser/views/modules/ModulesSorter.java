/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.modules;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

public class ModulesSorter extends ViewerSorter {

	@Override
	public int compare(final Viewer viewer, final Object e1, final Object e2) {
		if (e1 instanceof ModulesItem && e2 instanceof ModulesItem) {
			ModulesItem p1 = (ModulesItem)e1;
			ModulesItem p2 = (ModulesItem)e2;
			return p1.getShownName().compareToIgnoreCase(p2.getShownName());
		} else {
			return 0;
		}
	}
}
