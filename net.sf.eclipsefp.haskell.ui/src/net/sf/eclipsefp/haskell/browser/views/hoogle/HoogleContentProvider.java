/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.hoogle;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.browser.items.HoogleResult;
import net.sf.eclipsefp.haskell.browser.views.SpecialRoot;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

/**
 * Content provider for Hoogle results.
 * @author Alejandro Serrano, JP Moresmau
 *
 */
public class HoogleContentProvider implements ITreeContentProvider {

  List<Object> shownElements = null;


  public HoogleContentProvider() {

  }

  @Override
  public void inputChanged( final Viewer viewer, final Object oldInput,
      final Object newInput ) {

    if (newInput instanceof SpecialRoot){
      shownElements=Collections.singletonList( newInput );
    } else if (newInput instanceof HoogleSearchResult){
      HoogleSearchResult newResult = ( ( HoogleSearchResult )newInput );
      shownElements = newResult.getShownElements();
    } else {
      shownElements = null;
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
