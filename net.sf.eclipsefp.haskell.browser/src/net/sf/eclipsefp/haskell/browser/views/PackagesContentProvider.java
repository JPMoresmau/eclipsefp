package net.sf.eclipsefp.haskell.browser.views;

import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

public class PackagesContentProvider implements ITreeContentProvider {

	HaskellPackage[] localCache = null;

	public Object[] getElements(Object inputElement) {
		return new Object[] { DatabaseType.LOCAL };
	}

	public Object[] getChildren(Object parentElement) {
		if (localCache == null)
			cacheElements();

		switch ((DatabaseType) parentElement) {
		case LOCAL:
			return this.localCache;
		}

		return new Object[0];
	}

	public Object getParent(Object element) {
		if (element instanceof DatabaseType)
			return new PackagesRoot();
		else
			return DatabaseType.LOCAL;
	}

	public boolean hasChildren(Object element) {
		return (element instanceof PackagesRoot || element instanceof DatabaseType);
	}

	public void uncache() {
		this.localCache = null;
	}

	private void cacheElements() {
		try {
			BrowserPlugin.getSharedInstance().setCurrentDatabase(DatabaseType.LOCAL, null);
			this.localCache = BrowserPlugin.getSharedInstance().getPackages();
		} catch (Throwable ex) {
			this.localCache = new HaskellPackage[0];
		}
	}

	public void dispose() {
		// Do nothing
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		// Do nothing
	}
}
