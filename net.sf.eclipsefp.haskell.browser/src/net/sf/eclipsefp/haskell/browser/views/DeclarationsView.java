package net.sf.eclipsefp.haskell.browser.views;

import java.net.URL;

import net.sf.eclipsefp.haskell.browser.items.Constructor;
import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.DeclarationType;
import net.sf.eclipsefp.haskell.browser.items.PackageIdentifier;
import net.sf.eclipsefp.haskell.browser.items.Packaged;

import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.ui.part.ViewPart;

public abstract class DeclarationsView extends ViewPart implements ISelectionListener,
		ISelectionChangedListener, IDoubleClickListener {

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "net.sf.eclipsefp.haskell.browser.views.DeclarationsView";

	protected boolean isTypes;
	TreeViewer viewer;
	Browser doc;
	DeclarationsContentProvider provider;

	public DeclarationsView(boolean isTypes) {
		super();
		this.isTypes = isTypes;
	}

	@Override
	public void createPartControl(Composite parent) {
		SashForm form = new SashForm(parent, SWT.VERTICAL);
		viewer = new TreeViewer(form);
		doc = new Browser(form, SWT.NONE);
		form.setWeights(new int[] { 75, 25 });

		// Set label provider and sorter
		viewer.setLabelProvider(new DeclarationsLabelProvider());
		viewer.setSorter(new DeclarationsSorter());
		// Set content provider
		provider = new DeclarationsContentProvider(isTypes);
		viewer.setContentProvider(provider);
		viewer.setInput(null);
		// Hook for changes in selection
		viewer.addPostSelectionChangedListener(this);
		// Hook for double clicking
		viewer.addDoubleClickListener(this);
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

	// Last module loaded, needed for opening help
	ModulesItem lastModulesItem = null;

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
		if (o instanceof ModulesItem) {
			this.lastModulesItem = (ModulesItem) o;
			viewer.setInput(o);
			viewer.refresh();
		}
	}

	public void selectionChanged(SelectionChangedEvent event) {
		TreeSelection selection = (TreeSelection) event.getSelection();

		Object o = selection.getFirstElement();
		if (o == null) {
			doc.setText("");
			return;
		}

		if (o instanceof Packaged<?>) {
			Packaged<Declaration> decl = (Packaged<Declaration>) o;
			doc.setText(generateHtml(decl.getElement().getCompleteDefinition(), decl.getPackage(),
					decl.getElement().getDoc()));
		} else if (o instanceof Constructor) {
			Constructor c = (Constructor) o;
			doc.setText(generateHtml(c.getCompleteDefinition(), null, c.getDoc()));
		}
	}

	public String generateHtml(String definition, PackageIdentifier pkg, String docs) {
		StringBuilder builder = new StringBuilder();
		builder.append("<html>");
		builder.append("<body>");
		builder.append("<div style=\"font-size: small\">");

		builder.append("<p style=\"font-family: monospace\">");
		builder.append(definition);
		builder.append("</p>");

		String[] paragraphs = docs.split("\n\n");
		for (String paragraph : paragraphs) {
			builder.append("<p>");
			builder.append(paragraph);
			builder.append("</p>");
		}

		if (pkg != null) {
			builder.append("<p>");
			builder.append("<b>Defined in: </b>");
			builder.append(pkg.toString());
			builder.append("</p>");
		}

		builder.append("</div>");
		builder.append("</body>");
		builder.append("</html>");
		return builder.toString();
	}

	public void doubleClick(DoubleClickEvent event) {
		TreeSelection selection = (TreeSelection) event.getSelection();
		Object o = selection.getFirstElement();
		if (o == null || o instanceof Constructor)
			return;

		Packaged<Declaration> item = (Packaged<Declaration>) o;
		if (item.getElement().getType() == DeclarationType.INSTANCE)
			return; // No documentation for instances
		// Open browser
		try {
			IWorkbenchBrowserSupport browserSupport = this.getSite().getWorkbenchWindow()
					.getWorkbench().getBrowserSupport();
			URL webUrl = new URL(generateUrl(item));
			IWebBrowser browser = browserSupport.createBrowser(IWorkbenchBrowserSupport.AS_EDITOR
					| IWorkbenchBrowserSupport.LOCATION_BAR, null, "Haskell Browser",
					"Haskell Browser");
			browser.openURL(webUrl);
		} catch (Throwable ex) {

		}
	}

	public String generateUrl(Packaged<Declaration> item) {
		PackageIdentifier pkg = item.getPackage();
		String url;
		if (pkg.getName().equals("ghc")) {
			// GHC libraries are a special case
			url = "http://www.haskell.org/ghc/docs/" + pkg.getVersion() + "/html/libraries/"
					+ pkg.toString() + "/";
		} else {
			url = "http://hackage.haskell.org/packages/archive/" + pkg.getName() + "/"
					+ pkg.getVersion() + "/doc/html/";
		}
		// Add module name
		url += lastModulesItem.getModule().getName().replace('.', '-') + ".html";
		// Add declaration name
		if (item.getElement().getType() == DeclarationType.FUNCTION)
			url += "#v:" + item.getElement().getName();
		else
			url += "#t:" + item.getElement().getName();
		return url;
	}
}
