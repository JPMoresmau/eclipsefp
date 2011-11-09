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
	PackagesItem[] hackageCache = null;

	public Object[] getElements(final Object inputElement) {
	  boolean local = BrowserPlugin.getDefault().isLocalDatabaseLoaded();
	  boolean hackage = BrowserPlugin.getDefault().isHackageDatabaseLoaded();
	  if (local && hackage) {
      return new Object[] { DatabaseType.LOCAL, DatabaseType.HACKAGE };
    } else if (local) {
      return new Object[] { DatabaseType.LOCAL };
    } else if (hackage) {
      return new Object[] { DatabaseType.HACKAGE };
    } else {
      return new Object[0];
    }
	}

	public Object[] getChildren(final Object parentElement) {

		switch ((DatabaseType) parentElement) {
		case LOCAL:
		  if (localCache == null) {
	      cacheLocal();
	    }
			return this.localCache;
		case HACKAGE:
		  if (hackageCache == null) {
        cacheHackage();
      }
		  return this.hackageCache;
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
		this.hackageCache = null;
	}

	private void cacheLocal() {
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

	private void cacheHackage() {
    try {
      BrowserPlugin.getSharedInstance().setCurrentDatabase(DatabaseType.HACKAGE, null);
      ArrayList<PackagesItem> cache = new ArrayList<PackagesItem>();
      for (HaskellPackage pkg : BrowserPlugin.getSharedInstance().getPackages()) {
        cache.add(new PackagesItem(DatabaseType.HACKAGE, pkg));
      }
      this.hackageCache = cache.toArray(new PackagesItem[cache.size()]);
    } catch (Throwable ex) {
      this.hackageCache = new PackagesItem[0];
    }
  }

  public PackagesItem[] getLocalCache() {
    if (localCache == null) {
      cacheLocal();
    }
    return localCache;
  }

  public PackagesItem[] getHackageCache() {
    if (hackageCache == null) {
      cacheHackage();
    }
    return hackageCache;
  }

	public void dispose() {
		// Do nothing
	}

	public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
		// Do nothing
	}
}
