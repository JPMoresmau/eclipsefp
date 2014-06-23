/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.search;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import net.sf.eclipsefp.haskell.buildwrapper.types.SearchResultLocation;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer.HaskellResourceExtensionLP;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.search.internal.ui.text.NewTextSearchActionGroup;
import org.eclipse.search.ui.ISearchResult;
import org.eclipse.search.ui.ISearchResultViewPart;
import org.eclipse.search.ui.text.AbstractTextSearchViewPage;
import org.eclipse.search.ui.text.Match;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.actions.ActionContext;
import org.eclipse.ui.actions.ActionGroup;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.ui.model.WorkbenchViewerComparator;
import org.eclipse.ui.part.IShowInSource;
import org.eclipse.ui.part.IShowInTarget;
import org.eclipse.ui.part.IShowInTargetList;
import org.eclipse.ui.part.ShowInContext;
import org.eclipse.ui.texteditor.IDocumentProvider;


/**
 * Search page for Haskell usage searches: does the plumbing with the Eclipse search result handling
 * @author JP Moresmau
 *
 */
public class UsageSearchPage extends AbstractTextSearchViewPage implements IAdaptable {
  private UsageSearchResult lastResults;
  private ActionGroup fActionGroup;

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
    openAndSelect( page, ((SectionSearchResult)match.getElement()).getFile(), currentOffset, currentLength, activate );
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
    SearchResultLocation loc=((SectionSearchResult)match.getElement()).getLocations().iterator().next();
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

  private static final String[] SHOW_IN_TARGETS = { "org.eclipse.ui.views.ResourceNavigator" };
  private static final IShowInTargetList SHOW_IN_TARGET_LIST = new IShowInTargetList() {
    @Override
    public String[] getShowInTargetIds() {
      return SHOW_IN_TARGETS;
    }
  };

  /* (non-Javadoc)
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  @Override
  public Object getAdapter( @SuppressWarnings("rawtypes") final Class arg0 ) {
    if( arg0.equals( IShowInTarget.class ) ) {
      return SHOW_IN_TARGET_LIST;
    }
    if(arg0.equals( IShowInSource.class ) ) {
      ISelectionProvider selectionProvider = getSite().getSelectionProvider();
      if( selectionProvider == null ) {
        return null;
      }
      ISelection selection = selectionProvider.getSelection();
      if( selection instanceof IStructuredSelection ) {
        IStructuredSelection structuredSelection = ( IStructuredSelection )selection;
        final Set<Object> newSelection = new HashSet<>(
            structuredSelection.size() );

        Iterator<?> iter = structuredSelection.iterator();

        while( iter.hasNext() ) {

          Object element = iter.next();
          if (element instanceof SectionSearchResult){
            element = ((SectionSearchResult)element).getFile();
          }
          if (element!=null){
            newSelection.add( element );
          }
        }

        return new IShowInSource() {

            @Override
            public ShowInContext getShowInContext() {
              return new ShowInContext( null, new StructuredSelection(
                  new ArrayList<>( newSelection ) ) );
            }
          };


      }
      return null;
    }
    return null;
  }

  @Override
  public void setViewPart(final ISearchResultViewPart part) {
        super.setViewPart(part);
       this.fActionGroup = new NewTextSearchActionGroup(part);
   }

  @Override
  protected void fillContextMenu(final IMenuManager mgr) {
        super.fillContextMenu(mgr);
        this.fActionGroup.setContext(new ActionContext(getSite().getSelectionProvider().getSelection()));
        this.fActionGroup.fillContextMenu(mgr);
  }
}
