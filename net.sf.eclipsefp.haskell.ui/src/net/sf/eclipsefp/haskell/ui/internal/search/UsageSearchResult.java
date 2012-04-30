/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.search;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.buildwrapper.types.UsageResults;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.text.AbstractTextSearchResult;
import org.eclipse.search.ui.text.IEditorMatchAdapter;
import org.eclipse.search.ui.text.IFileMatchAdapter;
import org.eclipse.search.ui.text.Match;
import org.eclipse.search.ui.text.MatchEvent;


/**
 * Haskell Usage Search query result
 * @author JP Moresmau
 *
 */
public class UsageSearchResult extends AbstractTextSearchResult {
  private final ISearchQuery query;
 // private final List<ISearchResultListener> ll=new ArrayList<ISearchResultListener>();

  private UsageResults results=null;

  public UsageSearchResult(final ISearchQuery query) {
    super();
    this.query=query;
  }

  @Override
  public String getLabel() {
    return UITexts.References_result_label;
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

    final List<Match> matches=new ArrayList<Match>();
    if (this.results!=null){
      for (IProject p:this.results.listProjects()){
        Map<IFile,Collection<UsageResults.UsageLocation>> m=this.results.getUsageInProject( p );
        for (IFile f:m.keySet()){
          for (UsageResults.UsageLocation loc:m.get( f )){
            Match match=new Match(loc,Match.UNIT_LINE,loc.getStartLine(),loc.getLengthInLine());
            addMatch( match );
            matches.add(match);
          }
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
    // TODO Auto-generated method stub
    return null;
  }

  /* (non-Javadoc)
   * @see org.eclipse.search.ui.text.AbstractTextSearchResult#getEditorMatchAdapter()
   */
  @Override
  public IEditorMatchAdapter getEditorMatchAdapter() {
    // TODO Auto-generated method stub
    return null;
  }

}
