/**
 *  Copyright (c) 2014 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.browser.views.hoogle;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.Database;
import net.sf.eclipsefp.haskell.browser.items.HoogleResult;
import net.sf.eclipsefp.haskell.browser.items.HoogleResultConstructor;
import net.sf.eclipsefp.haskell.browser.items.HoogleResultDeclaration;
import net.sf.eclipsefp.haskell.browser.items.HoogleResultType;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.osgi.util.NLS;


/**
 * Hoogle search and results
 * @author JP Moresmau
 *
 */
public class HoogleSearchResult {
  private Map<String, ArrayList<HoogleResult>> results = null;
  private ArrayList<Object> shownElements = null;

  private final boolean localDbCheck;
  private final boolean hackageDbCheck;

  /**
   *
   */
  public HoogleSearchResult(final boolean localDbCheck,final boolean hackageDbCheck) {
    this.localDbCheck=localDbCheck;
    this.hackageDbCheck=hackageDbCheck;

  }


  /**
   * @return the results
   */
  public Map<String, ArrayList<HoogleResult>> getResults() {
    return results;
  }


  /**
   * @return the shownElements
   */
  public ArrayList<Object> getShownElements() {
    return shownElements;
  }

  public void search(final String newQuery){
    if( newQuery.length()==0 ) {
      results = null;
      shownElements = null;
    } else {
      try {
        // Get results depending of set of databases to check
        HoogleResult[] initialResults;

        if (localDbCheck && hackageDbCheck) {
          //BrowserPlugin.getSharedInstance().setCurrentDatabase( DatabaseType.ALL, null );
          initialResults = BrowserPlugin.queryHoogle(Database.ALL, newQuery );
        } else if (localDbCheck) {
          //BrowserPlugin.getSharedInstance().setCurrentDatabase( DatabaseType.LOCAL, null );
          initialResults = BrowserPlugin.queryHoogle(Database.LOCAL, newQuery );
        } else if (hackageDbCheck) {
          //BrowserPlugin.getSharedInstance().setCurrentDatabase( DatabaseType.HACKAGE, null );
          initialResults = BrowserPlugin.queryHoogle(Database.HACKAGE, newQuery );
        } else {
          initialResults = new HoogleResult[0];
        }

        results = new LinkedHashMap<>();
        for( HoogleResult result: initialResults ) {
          if (!result.getType().equals( HoogleResultType.WARNING )){
            String key = result.getCompleteDefinition();
            // Try to find element
            ArrayList<HoogleResult> entryList = results.get( key );
            // If we didn't find the key, add to list
            boolean found=false;
            if (entryList == null) {
              entryList = new ArrayList<>();
              results.put(key, entryList );
            } else if (result.getType().equals(HoogleResultType.DECLARATION)){
              for (HoogleResult hr:entryList){
                if (HoogleResultType.DECLARATION.equals( hr.getType() )){
                  if (((HoogleResultDeclaration)result).getModule()!=null && ((HoogleResultDeclaration)result).getModule().equals( ((HoogleResultDeclaration)hr).getModule())){
                    found=true;
                    break;
                  }
                }
              }
            } else if(result.getType().equals(HoogleResultType.CONSTRUCTOR)){
              for (HoogleResult hr:entryList){
                if (HoogleResultType.CONSTRUCTOR.equals( hr.getType() )){
                  if (((HoogleResultConstructor)result).getModule()!=null && ((HoogleResultConstructor)result).getModule().equals( ((HoogleResultConstructor)hr).getModule())){
                    found=true;
                    break;
                  }
                }
              }
            } else if (result.getType().equals( HoogleResultType.MODULE )){
              /*for (HoogleResult hr:entryList){
                if (HoogleResultType.MODULE.equals( hr.getType() )){
                  if (((HoogleResultModule)result).getPackageIdentifiers().size()!=null && ((HoogleResultModule)result).getModule().equals( ((HoogleResultModule)hr).getModule())){
                    found=true;
                    break;
                  }
                }
              }*/
              found=true;
            }
            if(!found){
              // Add element
              entryList.add(result);
            }
          } else {
            HaskellUIPlugin.log(NLS.bind( UITexts.browser_hoogleWarning, result.getName()),IStatus.WARNING );
          }

        }
        shownElements = new ArrayList<>();
        for( Map.Entry<String, ArrayList<HoogleResult>> entry: results.entrySet() ) {
          if( entry.getValue().size() == 1 ) {
            // If only one element, we introduce just the result
            shownElements.add( entry.getValue().get( 0 ) );
          } else {
            // If not, we introduce a (element name, list of elements) item
            shownElements.add( entry );
          }
        }
      } catch( Throwable ex ) {
        HaskellUIPlugin.log( ex );
        results = null;
        shownElements = null;
      }
    }
  }
}
