package net.sf.eclipsefp.haskell.ui.internal.views.outline;

import java.util.Collections;
import java.util.List;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

/**
 * <p>Manages a list of OutlineDef for Outline view</p>
  *
  * @author JP Moresmau
 */
public class OutlineCP implements ITreeContentProvider{
  private List<?> input;

  public Object[] getChildren( final Object parentElement ) {
   return new Object[0];
  }

  public Object getParent( final Object element ) {
    return null;
  }

  public boolean hasChildren( final Object element ) {
   return true;
  }

  public Object[] getElements( final Object inputElement ) {
     return input.toArray();
  }

  public void dispose() {
   input=null;

  }

  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput ) {
   if (newInput instanceof List<?>){
     input=(List<?>)newInput;
   } else {
     input=Collections.emptyList();
   }

  }

}
