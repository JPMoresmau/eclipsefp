/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.search;

import net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer.HaskellResourceExtensionLP;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.search.ui.ISearchResult;
import org.eclipse.search.ui.text.AbstractTextSearchViewPage;
import org.eclipse.ui.model.WorkbenchViewerComparator;


/**
 * Search page for Haskell usage searches
 * @author JP Moresmau
 *
 */
public class UsageSearchPage extends AbstractTextSearchViewPage {
  private UsageSearchResult lastResults;
//  private final ISearchResultListener listener=new ISearchResultListener() {
//
//    @Override
//    public void searchResultChanged( final SearchResultEvent paramSearchResultEvent ) {
//      new UIJob(UITexts.References_result_refreshing) {
//
//        @Override
//        public IStatus runInUIThread( final IProgressMonitor paramIProgressMonitor ) {
//          if (lastResults!=null){
//            getViewer().setInput( lastResults.getResults() );
//          }
//          return Status.OK_STATUS;
//        }
//      }.schedule();
//
//
//    }
//  };

  /* (non-Javadoc)
   * @see org.eclipse.search.ui.text.AbstractTextSearchViewPage#elementsChanged(java.lang.Object[])
   */
  @Override
  protected void elementsChanged( final Object[] paramArrayOfObject ) {
    getViewer().setInput( lastResults);
    //getViewer().refresh();
  }

  /* (non-Javadoc)
   * @see org.eclipse.search.ui.text.AbstractTextSearchViewPage#setInput(org.eclipse.search.ui.ISearchResult, java.lang.Object)
   */
  @Override
  public void setInput( final ISearchResult newSearch, final Object viewState ) {
    super.setInput( newSearch, viewState );
    final UsageSearchResult results=(UsageSearchResult)newSearch;
    lastResults=results;
  }

  /* (non-Javadoc)
   * @see org.eclipse.search.ui.text.AbstractTextSearchViewPage#setInput(org.eclipse.search.ui.ISearchResult, java.lang.Object)
   */
//  @Override
//  public void setInput( final ISearchResult newSearch, final Object viewState ) {
//    if (lastResults!=null){
//      lastResults.removeListener( listener );
//    }
//    lastResults=null;
//
//    final UsageSearchResult results=(UsageSearchResult)newSearch;
//    if (results!=null){
//      getViewer().setInput( results.getResults() );
//      lastResults=results;
//      lastResults.addListener( listener );
//    } else {
//      getViewer().setInput(null);
//    }
//  }

  /* (non-Javadoc)
   * @see org.eclipse.search.ui.text.AbstractTextSearchViewPage#clear()
   */
  @Override
  protected void clear() {
    // NOOP

  }

  /* (non-Javadoc)
   * @see org.eclipse.search.ui.text.AbstractTextSearchViewPage#configureTreeViewer(org.eclipse.jface.viewers.TreeViewer)
   */
  @Override
  protected void configureTreeViewer( final TreeViewer treeViewer ) {
    treeViewer.setUseHashlookup( true );
    treeViewer.setContentProvider( new UsageResultContentProvider() );
    treeViewer.setComparator( new WorkbenchViewerComparator() );
    treeViewer.setLabelProvider( new HaskellResourceExtensionLP() );
  }

  /* (non-Javadoc)
   * @see org.eclipse.search.ui.text.AbstractTextSearchViewPage#configureTableViewer(org.eclipse.jface.viewers.TableViewer)
   */
  @Override
  protected void configureTableViewer( final TableViewer tableViewer ) {
    // NOOP

  }

}
