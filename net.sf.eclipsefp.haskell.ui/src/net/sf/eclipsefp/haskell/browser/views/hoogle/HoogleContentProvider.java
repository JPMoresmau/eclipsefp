package net.sf.eclipsefp.haskell.browser.views.hoogle;

import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.items.HoogleResult;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;


public class HoogleContentProvider implements ITreeContentProvider {

  HoogleResult[] results = null;

  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput ) {
    if (newInput == null) {
      results = null;
      return;
    }

    String newQuery = ( ( String )newInput ).trim();
    if( newQuery.isEmpty() ) {
      results = null;
    } else {
      try {
        BrowserPlugin.getSharedInstance().setCurrentDatabase( DatabaseType.ALL,
            null );
        results = BrowserPlugin.getSharedInstance().queryHoogle( newQuery );
      } catch( Throwable ex ) {
        results = null;
      }
    }
  }

  public Object[] getElements( final Object inputElement ) {
    if (results == null) {
      return new Object[0];
    } else {
      return results;
    }
  }

  public Object getFirstElement() {
    if (results == null || results.length == 0) {
      return null;
    } else {
      return results[0];
    }
  }

  public Object[] getChildren( final Object parentElement ) {
    return new Object[0];
  }

  public Object getParent( final Object element ) {
    return null;
  }

  public boolean hasChildren( final Object element ) {
    return false;
  }

  public void dispose() {
    // Do nothing
  }
}
