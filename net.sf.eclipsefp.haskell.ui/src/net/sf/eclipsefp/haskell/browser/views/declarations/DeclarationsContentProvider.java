/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.declarations;

import java.util.ArrayList;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.Database;
import net.sf.eclipsefp.haskell.browser.items.Constructor;
import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.DeclarationType;
import net.sf.eclipsefp.haskell.browser.items.Gadt;
import net.sf.eclipsefp.haskell.browser.items.Packaged;
import net.sf.eclipsefp.haskell.browser.items.QueryItem;
import net.sf.eclipsefp.haskell.browser.views.modules.ModulesItem;
import net.sf.eclipsefp.haskell.browser.views.packages.PackagesItem;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

/**
 * Content provider for declarations.
 * Implements the nesting of instances in their parent elements.
 * @author Alejandro Serrano
 *
 */
public class DeclarationsContentProvider implements ITreeContentProvider {

  boolean isTypes;
  ArrayList<QueryItem> cache = null;

  public DeclarationsContentProvider( final boolean isTypes ) {
    this.isTypes = isTypes;
  }

  public void inputChanged( final Viewer viewer, final Object oldInput,
      final Object newInput ) {
    if( newInput == null || !( newInput instanceof ModulesItem ) ) {
      cache = new ArrayList<QueryItem>();
    } else {
      try {
        ModulesItem mitem = ( ModulesItem )newInput;

        // The module is a fake module from hierarchical view
        if( mitem.getModule() == null ) {
          cache = new ArrayList<QueryItem>();
          return;
        }

        Object o = mitem.getDatabaseInfo();
        Database db=Database.ALL;
        if( o instanceof Database ) {
          //BrowserPlugin.getSharedInstance().setCurrentDatabase(
          //    ( DatabaseType )o, null );
          db=(Database)o;
        } else {
          PackagesItem item = ( PackagesItem )o;
         // BrowserPlugin.getSharedInstance().setCurrentDatabase(
          //    DatabaseType.PACKAGE, item.getPackage().getIdentifier() );
          db=Database.Package( item.getPackage().getIdentifier() );
        }

        cache = new ArrayList<QueryItem>();
        Packaged<Declaration>[] decls = BrowserPlugin.getSharedInstance()
            .getDeclarations( db,mitem.getModule().getName() );
        for( QueryItem decl: QueryItem.convertToQueryItem( decls ) ) {
          if( decl.getType() == DeclarationType.FUNCTION && !isTypes ) {
            cache.add( decl );
          } else if( decl.getType() != DeclarationType.FUNCTION && isTypes ) {
            cache.add( decl );
          }
        }

      } catch( Throwable ex ) {
        cache = new ArrayList<QueryItem>();
      }
    }
  }

  public Object[] getElements( final Object inputElement ) {
    return cache.toArray();
  }

  public Object[] getChildren( final Object parentElement ) {
    if( parentElement instanceof QueryItem ) {
      QueryItem item = ( QueryItem )parentElement;
      ArrayList<Object> elements = new ArrayList<Object>();
      // If a GADT, add constructors
      if( item.getDeclaration() instanceof Gadt ) {
        for( Constructor c: ( ( Gadt )item.getDeclaration() ).getConstructors() ) {
          elements.add( c );
        }
      }
      // Add other inner items
      elements.addAll( item.getInnerItems() );
      return elements.toArray();
    } else {
      return new Object[ 0 ];
    }
  }

  public Object getParent( final Object element ) {
    return null;
  }

  public boolean hasChildren( final Object element ) {
    return this.getChildren( element ).length > 0;
  }

  public void dispose() {
    // Do nothing
  }
}
