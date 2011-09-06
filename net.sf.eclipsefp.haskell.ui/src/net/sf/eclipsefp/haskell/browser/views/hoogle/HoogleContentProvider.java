/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.hoogle;

import java.util.ArrayList;
import java.util.Map;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.items.HoogleResult;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

/**
 * Content provider for Hoogle results.
 * @author Alejandro Serrano
 *
 */
public class HoogleContentProvider implements ITreeContentProvider {

  ArrayList<Map.Entry<String, ArrayList<HoogleResult>>> results = null;
  ArrayList<Object> shownElements = null;

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
        BrowserPlugin.getSharedInstance().setCurrentDatabase( DatabaseType.ALL, null );
        HoogleResult[] initialResults = BrowserPlugin.getSharedInstance().queryHoogle( newQuery );
        results = new ArrayList<Map.Entry<String,ArrayList<HoogleResult>>>();
        for( HoogleResult result: initialResults ) {
          String key = result.getCompleteDefinition();
          // Try to find element
          ArrayList<HoogleResult> entryList = null;
          for (Map.Entry<String, ArrayList<HoogleResult>> middleResult : results) {
            if (middleResult.getKey().equals( key )) {
              entryList = middleResult.getValue();
              break;
            }
          }
          // If we didn't find the key, add to list
          if (entryList == null) {
            entryList = new ArrayList<HoogleResult>();
            results.add( new SimpleEntry<String, ArrayList<HoogleResult>>(key, entryList) );
          }
          // Add element
          entryList.add(result);
        }
        shownElements = new ArrayList<Object>();
        for( Map.Entry<String, ArrayList<HoogleResult>> entry: results ) {
          if( entry.getValue().size() == 1 ) {
            // If only one element, we introduce just the result
            shownElements.add( entry.getValue().get( 0 ) );
          } else {
            // If not, we introduce a (element name, list of elements) item
            shownElements.add( new SimpleEntry<String, ArrayList<HoogleResult>>( entry.getKey(), entry.getValue() ) );
          }
        }
      } catch( Throwable ex ) {
        HaskellUIPlugin.log( ex );
        results = null;
        shownElements = null;
      }
    }
  }

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


  @SuppressWarnings ( "unchecked" )
  public Object[] getChildren( final Object parentElement ) {
    if (parentElement instanceof Map.Entry) {
      Map.Entry<String, Object> entry = (Map.Entry<String, Object>)parentElement;
      if (entry.getValue() instanceof ArrayList) {
        ArrayList<Map.Entry<String, HoogleResult>> results = new ArrayList<Map.Entry<String,HoogleResult>>();
        for (HoogleResult result : (ArrayList<HoogleResult>)entry.getValue()) {
          results.add( new SimpleEntry( entry.getKey(), result ) );
        }
        return results.toArray();
      }
    }

    return new Object[0];
  }

  public Object getParent( final Object element ) {
    return null;
  }

  @SuppressWarnings ( "unchecked" )
  public boolean hasChildren( final Object element ) {
    if (element instanceof Map.Entry) {
      Map.Entry<String, Object> entry = (Map.Entry<String, Object>)element;
      return (entry.getValue() instanceof ArrayList);
    } else {
      return false;
    }
  }

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

    public K getKey() {
      return key;
    }

    public V getValue() {
      return value;
    }


    public void setKey( final K key ) {
      this.key = key;
    }


    public V setValue( final V value ) {
      V ret=this.value;
      this.value = value;
      return ret;
    }



  }
}
