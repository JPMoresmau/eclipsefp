package net.sf.eclipsefp.haskell.browser.views;

import net.sf.eclipsefp.haskell.browser.items.Constructor;
import net.sf.eclipsefp.haskell.browser.items.DeclarationType;
import net.sf.eclipsefp.haskell.browser.items.Instance;
import net.sf.eclipsefp.haskell.browser.items.QueryItem;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

public class DeclarationsSorter extends ViewerSorter {

  @Override
  public int compare( final Viewer viewer, final Object e1, final Object e2 ) {
    // Put constructors first
    if( e1 instanceof Constructor && e2 instanceof QueryItem ) {
      return -1;
    } else if( e1 instanceof QueryItem && e2 instanceof Constructor ) {
      return 1;
    } else if( e1 instanceof QueryItem && e2 instanceof QueryItem ) {
      QueryItem q1 = ( QueryItem )e1;
      QueryItem q2 = ( QueryItem )e2;
      // Put instances at the end
      if( q1.getType() != DeclarationType.INSTANCE
          && q2.getType() == DeclarationType.INSTANCE ) {
        return -1;
      } else if( q1.getType() == DeclarationType.INSTANCE
          && q2.getType() != DeclarationType.INSTANCE ) {
        return 1;
      } else if( q1.getType() == DeclarationType.INSTANCE
          && q2.getType() == DeclarationType.INSTANCE ) {
        Instance i1 = (Instance)q1.getDeclaration();
        Instance i2 = (Instance)q2.getDeclaration();
        return i1.getCompleteDefinition().compareToIgnoreCase( i2.getCompleteDefinition() );
      } else {
        return q1.getName().compareToIgnoreCase( q2.getName() );
      }
    } else if( e1 instanceof Constructor && e2 instanceof Constructor ) {
      Constructor d1 = ( Constructor )e1;
      Constructor d2 = ( Constructor )e2;
      return d1.getName().compareToIgnoreCase( d2.getName() );
    } else {
      return 0;
    }
  }
}
