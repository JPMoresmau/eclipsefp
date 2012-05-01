/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.search;

import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer.HaskellResourceExtensionLP;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.search.ui.ISearchResult;
import org.eclipse.search.ui.text.AbstractTextSearchViewPage;
import org.eclipse.search.ui.text.Match;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.ui.model.WorkbenchViewerComparator;
import org.eclipse.ui.texteditor.IDocumentProvider;


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
   lastResults=null;

  }

  /* (non-Javadoc)
   * @see org.eclipse.search.ui.text.AbstractTextSearchViewPage#showMatch(org.eclipse.search.ui.text.Match, int, int, boolean)
   */
  @Override
  protected void showMatch( final Match match, final int currentOffset, final int currentLength,
      final boolean activate ) throws PartInitException {
    IWorkbenchPage page = getSite().getPage();
    openAndSelect( page, ((Location)match.getElement()).getIFile(), currentOffset, currentLength, activate );
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

  /* (non-Javadoc)
   * @see org.eclipse.search.ui.text.AbstractTextSearchViewPage#getCurrentMatchLocation(org.eclipse.search.ui.text.Match)
   */
  @Override
  public IRegion getCurrentMatchLocation( final Match match ) {
    IDocumentProvider prov=new TextFileDocumentProvider();
    Location loc=(Location)match.getElement();
    try {
      prov.connect( loc.getIFile() );
      try {
        IDocument doc=prov.getDocument(  loc.getIFile() );
        int off1=loc.getStartOffset( doc );
        return new Region(off1,loc.getLength( doc ));
      } finally {
        prov.disconnect( loc.getIFile() );
      }
    } catch (Exception ce){
      HaskellUIPlugin.log( ce );
    }
    return super.getCurrentMatchLocation( match );
  }

}
