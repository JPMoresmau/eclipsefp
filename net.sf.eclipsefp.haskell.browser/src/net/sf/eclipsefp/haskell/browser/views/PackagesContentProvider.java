package net.sf.eclipsefp.haskell.browser.views;

import java.util.ArrayList;

import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

public class PackagesContentProvider implements ITreeContentProvider {
	
	PackagesItem[] localCache = null;

	public Object[] getElements(Object inputElement) {
		return new Object[] { DatabaseType.LOCAL };
	}

	public Object[] getChildren(Object parentElement) {
		if (localCache == null)
			cache();

		switch ((DatabaseType) parentElement) {
		case LOCAL:
			return this.localCache;
		}

		return new Object[0];
	}

	public Object getParent(Object element) {
		if (element instanceof DatabaseType) {
			return PackagesRoot.ROOT;
		} else {
			PackagesItem pkg = (PackagesItem)element;
			return pkg.getDatabase();
		}
	}

	public boolean hasChildren(Object element) {
		return (element instanceof PackagesRoot || element instanceof DatabaseType);
	}

	public void uncache() {
		this.localCache = null;
	}

	private void cache() {
		try {
			BrowserPlugin.getSharedInstance().setCurrentDatabase(DatabaseType.LOCAL, null);
			ArrayList<PackagesItem> cache = new ArrayList<PackagesItem>();
			for (HaskellPackage pkg : BrowserPlugin.getSharedInstance().getPackages())
				cache.add(new PackagesItem(DatabaseType.LOCAL, pkg));
			this.localCache = cache.toArray(new PackagesItem[cache.size()]);
		} catch (Throwable ex) {
			this.localCache = new PackagesItem[0];
		}
	}

	public void dispose() {
		// Do nothing
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		// Do nothing
	}
}
