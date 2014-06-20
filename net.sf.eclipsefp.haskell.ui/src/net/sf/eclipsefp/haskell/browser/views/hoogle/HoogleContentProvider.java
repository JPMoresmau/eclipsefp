/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
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
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.Button;

/**
 * Content provider for Hoogle results.
 * @author Alejandro Serrano
 *
 */
public class HoogleContentProvider implements ITreeContentProvider {

  Map<String, ArrayList<HoogleResult>> results = null;
  ArrayList<Object> shownElements = null;

  Button localDbCheck;
  Button hackageDbCheck;

  public HoogleContentProvider(final Button localDbCheck, final Button hackageDbCheck) {
    this.localDbCheck = localDbCheck;
    this.hackageDbCheck = hackageDbCheck;
  }

  @Override
  public void inputChanged( final Viewer viewer, final Object oldInput,
      final Object newInput ) {
    if( newInput == null || !(newInput instanceof String) ) {
      results = null;
      shownElements = null;
      return;
    }

    String newQuery = ( ( String )newInput ).trim();
    if( newQuery.length()==0 ) {
      results = null;
      shownElements = null;
    } else {
      try {
        // Get results depending of set of databases to check
        HoogleResult[] initialResults;
        if (localDbCheck.getSelection() && hackageDbCheck.getSelection()) {
          //BrowserPlugin.getSharedInstance().setCurrentDatabase( DatabaseType.ALL, null );
          initialResults = BrowserPlugin.getSharedInstance().queryHoogle(Database.ALL, newQuery );
        } else if (localDbCheck.getSelection()) {
          //BrowserPlugin.getSharedInstance().setCurrentDatabase( DatabaseType.LOCAL, null );
          initialResults = BrowserPlugin.getSharedInstance().queryHoogle(Database.LOCAL, newQuery );
        } else if (hackageDbCheck.getSelection()) {
          //BrowserPlugin.getSharedInstance().setCurrentDatabase( DatabaseType.HACKAGE, null );
          initialResults = BrowserPlugin.getSharedInstance().queryHoogle(Database.HACKAGE, newQuery );
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

  @Override
  public Object[] getElements( final Object inputElement ) {
    if( shownElements == null ) {
      return new Object[ 0 ];
    } else {
      return shownElements.toArray();
    }
  }

  public Object getFirstElement() {
    if( shownElements == null || shownElements.size() == 0 ) {
      return null;
    } else {
      return shownElements.get( 0 );
    }
  }


  @Override
  @SuppressWarnings ( "unchecked" )
  public Object[] getChildren( final Object parentElement ) {
    if (parentElement instanceof Map.Entry) {
      Map.Entry<String, Object> entry = (Map.Entry<String, Object>)parentElement;
      if (entry.getValue() instanceof ArrayList) {
        ArrayList<Map.Entry<String, HoogleResult>> results = new ArrayList<>();
        for (HoogleResult result : (ArrayList<HoogleResult>)entry.getValue()) {

            results.add( new SimpleEntry( entry.getKey(), result ) );

        }
        return results.toArray();
      }
    }

    return new Object[0];
  }

  @Override
  public Object getParent( final Object element ) {
    return null;
  }

  @Override
  @SuppressWarnings ( "unchecked" )
  public boolean hasChildren( final Object element ) {
    if (element instanceof Map.Entry) {
      Map.Entry<String, Object> entry = (Map.Entry<String, Object>)element;
      return (entry.getValue() instanceof ArrayList);
    } else {
      return false;
    }
  }

  @Override
  public void dispose() {
    // Do nothing
  }

  public class SimpleEntry<K, V> implements Map.Entry<K, V>{
    private K key;
    private V value;
    public SimpleEntry( final K key, final V value ) {
      super();
      this.key = key;
      this.value = value;
    }

    @Override
    public K getKey() {
      return key;
    }

    @Override
    public V getValue() {
      return value;
    }


    public void setKey( final K key ) {
      this.key = key;
    }


    @Override
    public V setValue( final V value ) {
      V ret=this.value;
      this.value = value;
      return ret;
    }



  }
}
