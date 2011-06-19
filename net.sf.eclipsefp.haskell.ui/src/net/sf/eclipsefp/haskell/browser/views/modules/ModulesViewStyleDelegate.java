package net.sf.eclipsefp.haskell.browser.views.modules;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;

public class ModulesViewStyleDelegate implements IViewActionDelegate {
	
	boolean isHierarchical;
	ModulesView view;
	
	public ModulesViewStyleDelegate(boolean isHierarchical) {
		this.isHierarchical = isHierarchical;
	}

	public void run(IAction action) {
		view.setHierarchical(this.isHierarchical);
	}

	public void selectionChanged(IAction action, ISelection selection) {
		// Do nothing
		
		if (view.getHierarchical() == this.isHierarchical)
			action.setChecked(true);
	}

	public void init(IViewPart view) {
		this.view = (ModulesView)view;
	}

}
