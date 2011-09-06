package net.sf.eclipsefp.haskell.profiler.internal.editors;

import net.sf.eclipsefp.haskell.profiler.internal.util.UITexts;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.PartInitException;

/**
 * empty implementation of profile viewer, in case Birt is not available
 * @author jean-philippem
 *
 */
public abstract class ProfileViewerImpl {

	
	public abstract void setPartName(String name);
	
	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		// NOOP
	}
	
	public void createPartControl(Composite parent) {
		Label l=new Label(parent,SWT.NONE);
		l.setText(UITexts.graph_requires_birt);
	}
	
	public boolean isSaveAsAllowed() {
		return false;
	}
	
	public void doSaveAs(Shell shell,String partName) {
		// noop
	}
}
