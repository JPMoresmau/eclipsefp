package net.sf.eclipsefp.haskell.ui.internal.views.outline;

import java.util.List;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineDef.OutlineDefType;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

/**
 * <p>Manages a list of OutlineDef for Outline view</p>
  *
  * @author JP Moresmau
 */
public class OutlineCP implements ITreeContentProvider{
  //private Map<String,List<OutlineDef>> input;
//  private List<OutlineDef> input;

  /**
   * For elements that expand into an identically-named single element with an obvious
   * type, we could just expand directly into that child element's children.
   */
  public boolean hasSingularChild( final Object o ){
    if( !((OutlineDef)o).getTypes().contains( OutlineDefType.DATA )) {
      return false;
    }

    Object[] children = getRawChildren( o );

    if( children.length != 1 ) {
      return false;
    }

    return ((OutlineDef)children[0]).getTypes().contains( OutlineDefType.CONSTRUCTOR) &&
           ((OutlineDef)children[0]).getName().equals( ((OutlineDef)o).getName() );
    }

  @Override
  public Object[] getChildren( final Object parentElement ) {
    Object[] result = getRawChildren( parentElement );
    if( hasSingularChild( parentElement ) ) {
      return getChildren( result[0] );
    } else {
      return result;
    }
  }

  public Object[] getRawChildren( final Object parentElement ) {
    //List<OutlineDef> l=input.get(((OutlineDef )parentElement).getID());
    if (parentElement instanceof OutlineDef){
      List<OutlineDef> l=((OutlineDef)parentElement).getChildren();
      if (l!=null){
        return l.toArray();
      }
    }
    return new Object[0];
  }

  @Override
  public Object getParent( final Object element ) {
    return null;
  }

  @Override
  public boolean hasChildren( final Object element ) {
    if (element instanceof OutlineDef){
      List<OutlineDef> l=((OutlineDef)element).getChildren();
      return l!=null && l.size()>0;
    }
   return true;
  }

  @Override
  public Object[] getElements( final Object inputElement ) {
     //return input.toArray();
    //List<OutlineDef> l=input.get( null );
    if (inputElement instanceof OutlineDef){
      List<OutlineDef> l=((OutlineDef)inputElement).getChildren();
      if (l!=null){
        return l.toArray();
      }
    } else if (inputElement instanceof List){
      return ((List<?>)inputElement).toArray();
    }

    return new Object[0];
  }

  @Override
  public void dispose() {
//   input=null;

  }

  @Override
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput ) {
//   if (newInput instanceof List<?>){
//     input=(List<OutlineDef>)newInput;
//    /*   new HashMap<String, List<OutlineDef>>();
//     for (Object o:(List<?>)newInput){
//       OutlineDef od=(OutlineDef)o;
//       List<OutlineDef> l=input.get( od.getParentID() );
//       if(l==null){
//         l=new ArrayList<OutlineDef>();
//         input.put( od.getParentID(), l );
//       }
//       l.add( od );
//     }*/
//   } else {
//     input=Collections.emptyList();
//   }

  }

}
