// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.mbview;

import java.util.*;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;


/** <p>Filters the elements displayed in the Module Browser, according to 
  * the criteria set in the filters dialog.</p>
  * 
  * @author Leif Frenzel
  */
public class ModuleBrowserFilter extends ViewerFilter {
  
  private final List<FilterCriterion> activeCriteria = new ArrayList<FilterCriterion>();
  
  public Object[] getActiveCriteria() {
    return activeCriteria.toArray();
  }
  
  public void setActiveCriteria( final Object[] criteria ) {
    activeCriteria.clear();
    for( int i = 0; i < criteria.length; i++ ) {
      if( criteria[ i ] instanceof FilterCriterion ) {
        activeCriteria.add( ( FilterCriterion )criteria[ i ] );
      }
    }
  }
  
  public Object[] getCriteria() {
    return FilterCriterion.ALL_CRITERIA;
  }
  
  
  // interface methods of ViewerFilter
  ////////////////////////////////////
  
  /** returns whether the given element makes it through this filter. */
  @Override
  public boolean select( final Viewer viewer, 
                         final Object parentElement, 
                         final Object element ) {
    boolean result = true;
    Iterator<FilterCriterion> iter = activeCriteria.iterator();
    while( result && iter.hasNext() ) {
      result = iter.next().matches( element );
    }
    return result;
  }
}