/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.packages;

import java.util.ArrayList;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.Database;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

/**
 * Content provider for packages view.
 * @author Alejandro Serrano
 *
 */
public class PackagesContentProvider implements ITreeContentProvider {

	private static PackagesItem[] localCache = null;
	private static PackagesItem[] hackageCache = null;

	public static void clearCache(){
	  localCache = null;
	  hackageCache = null;
	}

	@Override
  public Object[] getElements(final Object inputElement) {
	  boolean local = BrowserPlugin.getDefault().isLocalDatabaseLoaded();
	  boolean hackage = BrowserPlugin.getDefault().isHackageDatabaseLoaded();
	  if (local && hackage) {
      return new Object[] { Database.LOCAL, Database.HACKAGE };
    } else if (local) {
      return new Object[] { Database.LOCAL };
    } else if (hackage) {
      return new Object[] { Database.HACKAGE };
    } else {
      return new Object[0];
    }
	}

	@Override
  public Object[] getChildren(final Object parentElement) {

		switch (((Database) parentElement).getType()) {
		case LOCAL:
		  if (localCache == null) {
	      cacheLocal();
	    }
			return PackagesContentProvider.localCache;
		case HACKAGE:
		  if (hackageCache == null) {
        cacheHackage();
      }
		  return PackagesContentProvider.hackageCache;
		default:
		  return new Object[0];
		}


	}

	@Override
  public Object getParent(final Object element) {
		if (element instanceof Database) {
			return PackagesRoot.ROOT;
		} else {
			PackagesItem pkg = (PackagesItem)element;
			return pkg.getDatabase();
		}
	}

	@Override
  public boolean hasChildren(final Object element) {
		return (element instanceof PackagesRoot || element instanceof Database);
	}

	public void uncache() {
		PackagesContentProvider.localCache = null;
		PackagesContentProvider.hackageCache = null;
	}

	private void cacheLocal() {
		try {
			//BrowserPlugin.getSharedInstance().setCurrentDatabase(DatabaseType.LOCAL, null);
			ArrayList<PackagesItem> cache = new ArrayList<>();
			for (HaskellPackage pkg : BrowserPlugin.getSharedInstance().getPackages(Database.LOCAL)) {
        cache.add(new PackagesItem(Database.LOCAL, pkg));
      }
			PackagesContentProvider.localCache = cache.toArray(new PackagesItem[cache.size()]);
		} catch (Throwable ex) {
			PackagesContentProvider.localCache = new PackagesItem[0];
		}
	}

	private void cacheHackage() {
    try {
      //BrowserPlugin.getSharedInstance().setCurrentDatabase(DatabaseType.HACKAGE, null);
      ArrayList<PackagesItem> cache = new ArrayList<>();
      for (HaskellPackage pkg : BrowserPlugin.getSharedInstance().getPackages(Database.HACKAGE)) {
        cache.add(new PackagesItem(Database.HACKAGE, pkg));
      }
      PackagesContentProvider.hackageCache = cache.toArray(new PackagesItem[cache.size()]);
    } catch (Throwable ex) {
      PackagesContentProvider.hackageCache = new PackagesItem[0];
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

	@Override
  public void dispose() {
		// Do nothing
	}

	@Override
  public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
		// Do nothing
	}
}
