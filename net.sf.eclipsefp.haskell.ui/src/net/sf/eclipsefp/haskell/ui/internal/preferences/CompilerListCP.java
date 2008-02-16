// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.preferences;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;


/** <p>the content provider for the list of compilers registered with
  * the compiler manager.</p>
  * 
  * @author Leif Frenzel
  */
public class CompilerListCP implements IStructuredContentProvider {

  
  // interface methods of IStructuredContentProvider
  //////////////////////////////////////////////////
  
  public Object[] getElements( final Object inputElement ) {
    return CompilerManager.getInstance().getRegisteredCompilers();
  }
  
  public void dispose() {
    // unused
  }

  public void inputChanged( final Viewer viewer, 
                            final Object oldInput, 
                            final Object newInput ) {
    // unused
  }
}