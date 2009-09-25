package net.sf.eclipsefp.haskell.ui.internal.views.outline;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.scion.types.OutlineDef;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

/**
 * <p>Manages a list of OutlineDef for Outline view</p>
  *
  * @author JP Moresmau
 */
public class OutlineCP implements ITreeContentProvider{
  private Map<String,List<OutlineDef>> input;

  public Object[] getChildren( final Object parentElement ) {
    //return input.toArray();
    List<OutlineDef> l=input.get(((OutlineDef )parentElement).getID());
    if (l!=null){
      return l.toArray();
    }
    return new Object[0];
  }

  public Object getParent( final Object element ) {
    return null;
  }

  public boolean hasChildren( final Object element ) {
   return true;
  }

  public Object[] getElements( final Object inputElement ) {
     //return input.toArray();
    List<OutlineDef> l=input.get( null );
    if (l!=null){
      return l.toArray();
    }
    return new Object[0];
  }

  public void dispose() {
   input=null;

  }

  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput ) {
   if (newInput instanceof List<?>){
     input=new HashMap<String, List<OutlineDef>>();
     for (Object o:(List<?>)newInput){
       OutlineDef od=(OutlineDef)o;
       List<OutlineDef> l=input.get( od.getParentID() );
       if(l==null){
         l=new ArrayList<OutlineDef>();
         input.put( od.getParentID(), l );
       }
       l.add( od );
     }
   } else {
     input=Collections.emptyMap();
   }

  }

}
