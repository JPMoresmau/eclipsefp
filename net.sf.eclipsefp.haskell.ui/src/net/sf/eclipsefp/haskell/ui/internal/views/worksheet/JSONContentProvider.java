/**
 *  Copyright (c) 2014 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.views.worksheet;

import java.util.Iterator;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.model.WorkbenchViewerComparator;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;


/**
 * A JSON Content provider, to show JSON content in a tree
 * @author JP Moresmau
 *
 */
public class JSONContentProvider implements ITreeContentProvider {

  /**
   *
   */
  public JSONContentProvider() {
    // noop
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  @Override
  public void dispose() {
    // noop

  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object, java.lang.Object)
   */
  @Override
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput ) {
    // noop

  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getElements(java.lang.Object)
   */
  @Override
  public Object[] getElements( final Object inputElement ) {
    if (inputElement instanceof JSONObject){
      JSONObject obj=(JSONObject)inputElement;
      Object[] ret=new Object[obj.length()];
      int a=0;
      for (Iterator<String> it=obj.keys();it.hasNext();){
        String name=it.next();
        ret [a++]= new ObjectNode(obj,name);
      }
      return ret;
    } else if (inputElement instanceof JSONArray){
      JSONArray arr=(JSONArray)inputElement;
      Object[] ret=new Object[arr.length()];
      for (int a=0;a<arr.length();a++){
        ret[a]=new ArrayNode( arr, a );
      }
      return ret;
    }
    return new Object[]{inputElement};
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object parentElement ) {
    try {
      if (parentElement instanceof ObjectNode){
        ObjectNode on=(ObjectNode)parentElement;
        return getElements( on.obj.get( on.name ) );
      } else if (parentElement instanceof ArrayNode){
        ArrayNode an=(ArrayNode)parentElement;
        return getElements( an.arr.get( an.idx ) );
      }
    } catch (JSONException jsone){
      HaskellUIPlugin.log( jsone );
    }
    return null;
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  @Override
  public Object getParent( final Object element ) {
    return null;
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  @Override
  public boolean hasChildren( final Object element ) {
    if (element instanceof JSONObject){
      return ((JSONObject)element).length()>0;

    } else if (element instanceof ObjectNode){
      return true;
    } else if (element instanceof JSONArray){
      return ((JSONArray)element).length()>0;
    }else if (element instanceof ArrayNode){
      return true;
    }
    return false;
  }


  /**
   * Wraps the JSONObject + name of key
   * @author JP Moresmau
   *
   */
  private static class ObjectNode {
    JSONObject obj;
    String name;

    public ObjectNode( final JSONObject obj, final String name ) {
      super();
      this.obj = obj;
      this.name = name;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
      return name;
    }

    @Override
    public int hashCode() {
      final int prime = 31;
      int result = 1;
      result = prime * result + ( ( name == null ) ? 0 : name.hashCode() );
      result = prime * result + ( ( obj == null ) ? 0 : obj.hashCode() );
      return result;
    }

    /**
     * we know that JSONObject doesn't implement equals, but that's fine, we want pointer equality on the JSON object
     */
    @Override
    public boolean equals( final Object obj ) {
      if( this == obj ) {
        return true;
      }
      if( obj == null ) {
        return false;
      }
      if( getClass() != obj.getClass() ) {
        return false;
      }
      ObjectNode other = ( ObjectNode )obj;
      if( name == null ) {
        if( other.name != null ) {
          return false;
        }
      } else if( !name.equals( other.name ) ) {
        return false;
      }
      if( this.obj == null ) {
        if( other.obj != null ) {
          return false;
        }
      } else if( !this.obj.equals( other.obj ) ) {
        return false;
      }
      return true;
    }
  }

  /**
   * Wraps a JSONArray + index of value
   * @author JP Moresmau
   *
   */
  private static class ArrayNode {
    JSONArray arr;
    int idx;

    public ArrayNode( final JSONArray arr, final int idx ) {
      super();
      this.arr = arr;
      this.idx = idx;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
       return String.valueOf( idx );
    }

    @Override
    public int hashCode() {
      final int prime = 31;
      int result = 1;
      result = prime * result + ( ( arr == null ) ? 0 : arr.hashCode() );
      result = prime * result + idx;
      return result;
    }

    /**
     * we know that JSONArray doesn't implement equals, but that's fine, we want pointer equality on the JSON array
     */
    @Override
    public boolean equals( final Object obj ) {
      if( this == obj ) {
        return true;
      }
      if( obj == null ) {
        return false;
      }
      if( getClass() != obj.getClass() ) {
        return false;
      }
      ArrayNode other = ( ArrayNode )obj;
      if( arr == null ) {
        if( other.arr != null ) {
          return false;
        }
      } else if( !arr.equals( other.arr ) ) {
        return false;
      }
      if( idx != other.idx ) {
        return false;
      }
      return true;
    }

  }

  /**
   * Ensure JSON Array values are sorted properly by index
   * @author JP Moresmau
   *
   */
  public static class JSONComparator extends WorkbenchViewerComparator {
    /* (non-Javadoc)
     * @see org.eclipse.jface.viewers.ViewerComparator#compare(org.eclipse.jface.viewers.Viewer, java.lang.Object, java.lang.Object)
     */
    @Override
    public int compare( final Viewer viewer, final Object e1, final Object e2 ) {
      if (e1 instanceof ArrayNode && e2 instanceof ArrayNode){
        ArrayNode an1=(ArrayNode)e1;
        ArrayNode an2=(ArrayNode)e2;
        return Integer.valueOf( an1.idx).compareTo(an2.idx);
      }
      return super.compare( viewer, e1, e2 );
    }
  }

}
