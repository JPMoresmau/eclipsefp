// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.ui.preferences;

import net.sf.eclipsefp.haskell.ghccompiler.core.GhcParameter;
import net.sf.eclipsefp.haskell.ghccompiler.core.GhcParameterType;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/** <p>Tab for Language options on the ghc preference page.</p>
  *
  * @author Leif Frenzel
  */
public class LanguageTab extends GhcCompilerTab {

  public LanguageTab( final IPreferenceStore store ) {
    super( store );
  }


  // interface methods of Tab
  ///////////////////////////

  @Override
  public Control createControl( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );
    for (GhcParameter p:GhcParameter.values()){
      if (GhcParameterType.LANGUAGE.equals( p.getType() )){
        createBooleanField( composite, p );
      }
    }

    return composite;
  }
}