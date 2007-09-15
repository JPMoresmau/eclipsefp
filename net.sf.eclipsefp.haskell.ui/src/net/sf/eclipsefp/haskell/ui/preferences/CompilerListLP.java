// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.preferences;

import org.eclipse.jface.viewers.LabelProvider;

import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;


/** <p>a label provider for compilers from the core.</p>
  * 
  * @author Leif Frenzel
  */
public class CompilerListLP extends LabelProvider {

  @Override
  public String getText( final Object element ) {
    String compilerId = ( String )element;
    return CompilerManager.getInstance().getCompilerName( compilerId );
  }
}