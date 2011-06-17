package net.sf.eclipsefp.haskell.browser.views;

import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.DatabaseLoadedEvent;
import net.sf.eclipsefp.haskell.browser.IDatabaseLoadedListener;

import org.eclipse.jface.text.TextViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.part.ViewPart;


public class PackagesView extends ViewPart implements IDatabaseLoadedListener {

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "net.sf.eclipsefp.haskell.browser.views.PackagesView";
	
	TreeViewer viewer;
	TextViewer doc;
	PackagesContentProvider provider;

	@Override
	public void createPartControl(Composite parent) {
		SashForm form = new SashForm(parent, SWT.VERTICAL);
		viewer = new TreeViewer(form);
		doc = new TextViewer(form, SWT.NONE);
		form.setWeights(new int[] { 80, 20 });
		
		// Set label provider and sorter
		viewer.setLabelProvider(new PackagesLabelProvider());
		viewer.setSorter(new PackagesSorter());
		// Set initial content provider
		provider = new PackagesContentProvider();
		viewer.setContentProvider(provider);
		viewer.setInput(PackagesRoot.ROOT);
		// Hook for listeners
		BrowserPlugin.getDefault().addDatabaseLoadedListener(this);
		
		// Register as selection provider
		getSite().setSelectionProvider(viewer);
	}

	@Override
	public void setFocus() {
		viewer.getControl().setFocus();
	}

	public void databaseLoaded(DatabaseLoadedEvent e) {
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				// Use the new provider
				provider.uncache();
				viewer.refresh();
			}
		});
	}
}