package net.sf.eclipsefp.haskell.browser.views;

import java.util.ArrayList;

import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.DeclarationType;
import net.sf.eclipsefp.haskell.browser.items.Gadt;
import net.sf.eclipsefp.haskell.browser.items.Packaged;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

public class DeclarationsContentProvider implements ITreeContentProvider {

	boolean isTypes;
	ArrayList<Packaged<Declaration>> cache = null;

	public DeclarationsContentProvider(boolean isTypes) {
		this.isTypes = isTypes;
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		if (newInput == null || !(newInput instanceof ModulesItem)) {
			cache = new ArrayList<Packaged<Declaration>>();
		} else {
			try {
				ModulesItem mitem = (ModulesItem)newInput;
				
				// The module is a fake module from hierarchical view
				if (mitem.getModule() == null) {
					cache = new ArrayList<Packaged<Declaration>>();
					return;
				}

				Object o = mitem.getDatabaseInfo();
				if (o instanceof DatabaseType) {
					BrowserPlugin.getSharedInstance().setCurrentDatabase((DatabaseType) o, null);
				} else {
					PackagesItem item = (PackagesItem) o;
					BrowserPlugin.getSharedInstance().setCurrentDatabase(DatabaseType.PACKAGE,
							item.getPackage().getIdentifier());
				}
				
				cache = new ArrayList<Packaged<Declaration>>();
				for (Packaged<Declaration> decl : BrowserPlugin.getSharedInstance().getDeclarations(mitem.getModule().getName())) {
					System.out.println(decl.getElement().getName());
					if (decl.getElement().getType() == DeclarationType.FUNCTION && !isTypes)
						cache.add(decl);
					else if (decl.getElement().getType() != DeclarationType.FUNCTION && isTypes)
						cache.add(decl);
				}

			} catch (Throwable ex) {
				cache = new ArrayList<Packaged<Declaration>>();
			}
		}
	}

	public Object[] getElements(Object inputElement) {
		return cache.toArray();
	}

	public Object[] getChildren(Object parentElement) {
		if (parentElement instanceof Gadt) {
			Gadt item = (Gadt) parentElement;
			return item.getConstructors();
		}
		return new Object[0];
	}

	public Object getParent(Object element) {
		return null;
	}

	public boolean hasChildren(Object element) {
		// Only datatypes and newtypes may have children
		if (element instanceof Gadt) {
			Gadt item = (Gadt) element;
			return item.getConstructors().length > 0;
		}

		return false;
	}

	public void dispose() {
		// Do nothing
	}
}
