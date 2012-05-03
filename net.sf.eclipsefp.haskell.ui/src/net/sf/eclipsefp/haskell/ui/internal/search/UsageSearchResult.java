/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.search;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.buildwrapper.types.UsageResults;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.osgi.util.NLS;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.text.AbstractTextSearchResult;
import org.eclipse.search.ui.text.IEditorMatchAdapter;
import org.eclipse.search.ui.text.IFileMatchAdapter;
import org.eclipse.search.ui.text.Match;
import org.eclipse.search.ui.text.MatchEvent;
import org.eclipse.ui.IEditorPart;


/**
 * Haskell Usage Search query result
 * @author JP Moresmau
 *
 */
public class UsageSearchResult extends AbstractTextSearchResult {
  private final ISearchQuery query;
 // private final List<ISearchResultListener> ll=new ArrayList<ISearchResultListener>();

  private UsageResults results=null;
  private final Map<IFile,Match[]> matchByFile=new HashMap<IFile, Match[]>();

  private final String search;
  private final IProject project;

  public UsageSearchResult(final ISearchQuery query,final String search,final IProject project) {
    super();
    this.query=query;
    this.search=search;
    this.project=project;
  }

  @Override
  public String getLabel() {
    if (results==null){
      return UITexts.References_result_label;
    }
    int sz=results.getSize();
    if (project==null){

      if (sz<2){
        return NLS.bind( UITexts.References_result_label_worskpace_single, search,sz );
      } else {
        return NLS.bind( UITexts.References_result_label_worskpace, search,sz );
      }
    }
    if (sz<2){
      return NLS.bind( UITexts.References_result_label_project_single, new Object[]{search,sz,project.getName() });
    } else {
      return NLS.bind( UITexts.References_result_label_project, new Object[]{search,sz,project.getName() });
    }
  }

  @Override
  public String getTooltip() {
    return UITexts.References_result_tooltip;
  }

  @Override
  public ImageDescriptor getImageDescriptor() {
    return HaskellUIImages.getImageDescriptor( IImageNames.HASKELL_MISC );
  }

  @Override
  public ISearchQuery getQuery() {
    return query;
  }

  /* (non-Javadoc)
   * @see org.eclipse.search.ui.ISearchResult#addListener(org.eclipse.search.ui.ISearchResultListener)
   */
//  @Override
//  public void addListener( final ISearchResultListener paramISearchResultListener ) {
//    ll.add( paramISearchResultListener );
//
//  }

  /* (non-Javadoc)
   * @see org.eclipse.search.ui.ISearchResult#removeListener(org.eclipse.search.ui.ISearchResultListener)
   */
//  @Override
//  public void removeListener( final ISearchResultListener paramISearchResultListener ) {
//    ll.remove( paramISearchResultListener );
//
//  }


  public UsageResults getResults() {
    return results;
  }

  public void setResults( final UsageResults results ) {
    this.results = results;
    matchByFile.clear();
    final List<Match> matches=new ArrayList<Match>();
    if (this.results!=null){
      for (IProject p:this.results.listProjects()){
        Map<IFile,Collection<Location>> m=this.results.getUsageInProject( p );
        for (IFile f:m.keySet()){
          List<Match> myMatches=new ArrayList<Match>();
          for (Location loc:m.get( f )){
            Match match=new Match(loc,Match.UNIT_LINE,loc.getStartLine()-1,1);
            addMatch( match );
            myMatches.add(match);
          }
          matchByFile.put( f, myMatches.toArray( new Match[myMatches.size()] ) );
          matches.addAll(myMatches);
        }
      }
    }
    MatchEvent me=new MatchEvent( this ) {
      /* (non-Javadoc)
       * @see org.eclipse.search.ui.text.MatchEvent#getMatches()
       */
      @Override
      public Match[] getMatches() {
        return matches.toArray( new Match[matches.size()] );
      }
    };

    fireChange( me);

//    for (ISearchResultListener l:ll){
//      l.searchResultChanged( new SearchResultEvent(this){
//
//        /**
//         *
//         */
//        private static final long serialVersionUID = 5711720505168515138L;
//
//      } );
//    }
  }

  /* (non-Javadoc)
   * @see org.eclipse.search.ui.text.AbstractTextSearchResult#getFileMatchAdapter()
   */
  @Override
  public IFileMatchAdapter getFileMatchAdapter() {
    return new FileMatchAdapter() ;
  }

  /* (non-Javadoc)
   * @see org.eclipse.search.ui.text.AbstractTextSearchResult#getEditorMatchAdapter()
   */
  @Override
  public IEditorMatchAdapter getEditorMatchAdapter() {
    return new EditorMatchAdapter();
  }

  private static class FileMatchAdapter implements IFileMatchAdapter{
    @Override
    public IFile getFile( final Object paramObject ) {
      if (paramObject instanceof Location){
        return ((Location)paramObject).getIFile();
      }
      return null;
    }

    @Override
    public Match[] computeContainedMatches(
        final AbstractTextSearchResult paramAbstractTextSearchResult, final IFile paramIFile ) {
      Match[] ms= ((UsageSearchResult)paramAbstractTextSearchResult).matchByFile.get( paramIFile );
      if (ms==null){
        ms=new Match[0];
      }
      return ms;
    }
  }

  private static class EditorMatchAdapter implements IEditorMatchAdapter {
    /* (non-Javadoc)
     * @see org.eclipse.search.ui.text.IEditorMatchAdapter#isShownInEditor(org.eclipse.search.ui.text.Match, org.eclipse.ui.IEditorPart)
     */
    @Override
    public boolean isShownInEditor( final Match paramMatch,
        final IEditorPart paramIEditorPart ) {
      if (paramIEditorPart instanceof HaskellEditor){
        IFile f=((Location)paramMatch.getElement()).getIFile();
        return f.equals(((HaskellEditor)paramIEditorPart).findFile());
      }
      return false;
    }

    /* (non-Javadoc)
     * @see org.eclipse.search.ui.text.IEditorMatchAdapter#computeContainedMatches(org.eclipse.search.ui.text.AbstractTextSearchResult, org.eclipse.ui.IEditorPart)
     */
    @Override
    public Match[] computeContainedMatches(
        final AbstractTextSearchResult paramAbstractTextSearchResult,
        final IEditorPart paramIEditorPart ) {
      Match[] ms=null;
      if (paramIEditorPart instanceof HaskellEditor){
        IFile f=((HaskellEditor)paramIEditorPart).findFile();
        ms= ((UsageSearchResult)paramAbstractTextSearchResult).matchByFile.get( f );
      }
      if (ms==null){
        ms=new Match[0];
      }
      return ms;
    }
  }

}
