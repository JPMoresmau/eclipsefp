package net.sf.eclipsefp.haskell.browser.views;

import org.eclipse.jface.text.TextViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.part.ViewPart;

public abstract class DeclarationsView extends ViewPart implements ISelectionListener {

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "net.sf.eclipsefp.haskell.browser.views.DeclarationsView";

	protected boolean isTypes;
	TreeViewer viewer;
	TextViewer doc;
	DeclarationsContentProvider provider;
	
	public DeclarationsView(boolean isTypes) {
		super();
		this.isTypes = isTypes;
	}

	@Override
	public void createPartControl(Composite parent) {
		SashForm form = new SashForm(parent, SWT.VERTICAL);
		viewer = new TreeViewer(form);
		doc = new TextViewer(form, SWT.NONE);
		form.setWeights(new int[] { 80, 20 });

		// Set label provider and sorter
		viewer.setLabelProvider(new DeclarationsLabelProvider());
		viewer.setSorter(new DeclarationsSorter());
		// Set content provider
		provider = new DeclarationsContentProvider(isTypes);
		viewer.setContentProvider(provider);
		viewer.setInput(null);

		// Hook onto selection changes
		getSite().getPage().addPostSelectionListener(this);
	}

	@Override
	public void setFocus() {
		viewer.getControl().setFocus();
	}

	@Override
	public void dispose() {
		getSite().getPage().removePostSelectionListener(this);
		super.dispose();
	}

	// This will be called when a new package is selected
	public void selectionChanged(IWorkbenchPart part, ISelection selection) {
		if (part == this)
			return;
		if (!(selection instanceof IStructuredSelection))
			return;
		IStructuredSelection sel = (IStructuredSelection) selection;
		Object o = sel.getFirstElement();
		if (o == null)
			return;
		if (o instanceof ModulesItem)
			viewer.setInput(o);
	}
}
