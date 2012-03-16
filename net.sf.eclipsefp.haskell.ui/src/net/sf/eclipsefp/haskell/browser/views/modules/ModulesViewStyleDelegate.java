/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.modules;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;

/**
 * Action that changes the way modules are shown in the
 * view: either flat or hierarchical.
 * @author Alejandro Serrano
 *
 */
public class ModulesViewStyleDelegate implements IViewActionDelegate {

	boolean isHierarchical;
	ModulesView view;

	public ModulesViewStyleDelegate(final boolean isHierarchical) {
		this.isHierarchical = isHierarchical;
	}

	@Override
  public void run(final IAction action) {
		view.setHierarchical(this.isHierarchical);
	}

	@Override
  public void selectionChanged(final IAction action, final ISelection selection) {
		// Do nothing

		if (view.getHierarchical() == this.isHierarchical) {
      action.setChecked(true);
    }
	}

	@Override
  public void init(final IViewPart view) {
		this.view = (ModulesView)view;
	}

}
