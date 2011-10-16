/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.packages;

import java.util.ArrayList;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

/**
 * Content provider for packages view.
 * @author Alejandro Serrano
 *
 */
public class PackagesContentProvider implements ITreeContentProvider {

	PackagesItem[] localCache = null;

	public Object[] getElements(final Object inputElement) {
		return new Object[] { DatabaseType.LOCAL };
	}

	public Object[] getChildren(final Object parentElement) {
		if (localCache == null) {
      cache();
    }

		switch ((DatabaseType) parentElement) {
		case LOCAL:
			return this.localCache;
		default:
		  return new Object[0];
		}


	}

	public Object getParent(final Object element) {
		if (element instanceof DatabaseType) {
			return PackagesRoot.ROOT;
		} else {
			PackagesItem pkg = (PackagesItem)element;
			return pkg.getDatabase();
		}
	}

	public boolean hasChildren(final Object element) {
		return (element instanceof PackagesRoot || element instanceof DatabaseType);
	}

	public void uncache() {
		this.localCache = null;
	}

	private void cache() {
		try {
			BrowserPlugin.getSharedInstance().setCurrentDatabase(DatabaseType.LOCAL, null);
			ArrayList<PackagesItem> cache = new ArrayList<PackagesItem>();
			for (HaskellPackage pkg : BrowserPlugin.getSharedInstance().getPackages()) {
        cache.add(new PackagesItem(DatabaseType.LOCAL, pkg));
      }
			this.localCache = cache.toArray(new PackagesItem[cache.size()]);
		} catch (Throwable ex) {
			this.localCache = new PackagesItem[0];
		}
	}


  public PackagesItem[] getLocalCache() {
    if (localCache == null) {
      cache();
    }
    return localCache;
  }

	public void dispose() {
		// Do nothing
	}

	public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
		// Do nothing
	}
}
