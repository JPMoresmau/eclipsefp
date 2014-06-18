/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.search;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import net.sf.eclipsefp.haskell.buildwrapper.types.SearchResultLocation;
import net.sf.eclipsefp.haskell.buildwrapper.types.UsageResults;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;


/**
 * Content provider for search results
 * @author JP Moresmau
 *
 */
public class UsageResultContentProvider implements ITreeContentProvider {
  /**
   * children by parents
   */
  private Map<Object,Collection<Object>> uiresults=new HashMap<>();
  /**
   * roots
   */
  private Object[] roots=new Object[0];

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  @Override
  public void dispose() {
    uiresults=null;
    roots=null;
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object, java.lang.Object)
   */
  @Override
  public void inputChanged( final Viewer arg0, final Object arg1, final Object arg2 ) {
    uiresults.clear();
    roots=new Object[0];
    if (arg2 instanceof UsageSearchResult){
      UsageResults results=((UsageSearchResult)arg2).getResults();
      if (results!=null){
        Set<IProject> ps=results.listProjects();
        roots=ps.toArray( new Object[ps.size()] );
        for (IProject p:ps){
          Map<IFile,Map<String,Collection<SearchResultLocation>>> m=results.getUsageInProject( p );
          for (IFile f:m.keySet()){
            Map<String,Collection<SearchResultLocation>> uls=m.get( f );
            Collection<Object> l=new ArrayList<>();
            for (String sec:uls.keySet()){
              l.add(new SectionSearchResult( f,sec, uls.get( sec ) ));
            }
            uiresults.put( f, l);
            IContainer parent=f.getParent();
            Object child=f;
            while (!(parent instanceof IWorkspaceRoot)){
              add(parent,child);
              child=parent;
              parent=parent.getParent();
            }
          }
        }
      }
    }
  }

  private void add(final Object parent,final Object child){
    Collection<Object> cs=uiresults.get( parent );
    if (cs==null){
      cs=new HashSet<>();
      uiresults.put( parent, cs );
    }
    cs.add( child );
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object arg0 ) {
    Collection<Object> c=uiresults.get( arg0 );
    if (c!=null){
      return c.toArray( new Object[c.size()] );
    }
    return null;
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getElements(java.lang.Object)
   */
  @Override
  public Object[] getElements( final Object arg0 ) {
    return roots;
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  @Override
  public Object getParent( final Object arg0 ) {
    // unused
    return null;
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  @Override
  public boolean hasChildren( final Object arg0 ) {
    return arg0 instanceof IProject || arg0 instanceof IFile  || arg0 instanceof IFolder;
  }

}
