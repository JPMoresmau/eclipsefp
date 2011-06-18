package net.sf.eclipsefp.haskell.browser.views;

import java.net.URL;
import java.util.ArrayList;
import net.sf.eclipsefp.haskell.browser.items.Constructor;
import net.sf.eclipsefp.haskell.browser.items.DeclarationType;
import net.sf.eclipsefp.haskell.browser.items.PackageIdentifier;
import net.sf.eclipsefp.haskell.browser.items.QueryItem;
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

	public DeclarationsView(final boolean isTypes) {
		super();
		this.isTypes = isTypes;
	}

	@Override
	public void createPartControl(final Composite parent) {
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
	public void selectionChanged(final IWorkbenchPart part, final ISelection selection) {
		if (part == this) {
      return;
    }
		if (!(selection instanceof IStructuredSelection)) {
      return;
    }
		IStructuredSelection sel = (IStructuredSelection) selection;
		Object o = sel.getFirstElement();
		if (o == null) {
      return;
    }
		if (o instanceof PackagesItem) {
		  viewer.setInput( null );
		  viewer.refresh();
		}
		if (o instanceof ModulesItem) {
			this.lastModulesItem = (ModulesItem) o;
			viewer.setInput(o);
			viewer.refresh();
		}
	}

	public void selectionChanged(final SelectionChangedEvent event) {
		TreeSelection selection = (TreeSelection) event.getSelection();

		Object o = selection.getFirstElement();
		if (o == null) {
			doc.setText("");
			return;
		}

		if (o instanceof QueryItem) {
			QueryItem decl = (QueryItem) o;
			doc.setText(generateHtml(decl.getDeclaration().getCompleteDefinition(), decl.getPackages(),
					decl.getDeclaration().getDoc()));
		} else if (o instanceof Constructor) {
			Constructor c = (Constructor) o;
			doc.setText(generateHtml(c.getCompleteDefinition(), null, c.getDoc()));
		}
	}

	public String generateHtml(final String definition, final ArrayList<PackageIdentifier> pkgs, final String docs) {
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

		if (pkgs != null) {
			builder.append("<p>");
			builder.append("<b>Defined in: </b>");
			boolean first = true;
			for (PackageIdentifier pkg : pkgs) {
			  if (!first) {
			    builder.append(", ");
			  }
			  builder.append(pkg.toString());
			  first = false;
			}
			builder.append("</p>");
		}

		builder.append("</div>");
		builder.append("</body>");
		builder.append("</html>");
		return builder.toString();
	}

	public void doubleClick(final DoubleClickEvent event) {
		TreeSelection selection = (TreeSelection) event.getSelection();
		Object o = selection.getFirstElement();
		if (o == null || o instanceof Constructor) {
      return;
    }

		QueryItem item = (QueryItem) o;
		if (item.getDeclaration().getType() == DeclarationType.INSTANCE)
     {
      return; // No documentation for instances
    }
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
		  // Do nothing
		}
	}

	public String generateUrl(final QueryItem item) {
		PackageIdentifier pkg = item.getPackages().get( 0 );
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
		if (item.getType() == DeclarationType.FUNCTION) {
      url += "#v:" + item.getName();
    } else {
      url += "#t:" + item.getName();
    }
		return url;
	}
}
