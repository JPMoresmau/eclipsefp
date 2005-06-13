// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ghccompiler.ui.preferences;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import de.leiffrenzel.fp.haskell.ghccompiler.core.IGhcParameters;


/** <p>Tab for Language options on the ghc preference page.</p>
  * 
  * @author Leif Frenzel
  */
public class LanguageTab extends GhcCompilerTab implements IGhcParameters {
  
  public LanguageTab( final IPreferenceStore store ) {
    super( store );
  }

  
  // interface methods of Tab
  ///////////////////////////
  
  public Control createControl( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );
    
    createBooleanField( composite, LANG_GLASGOW_EXTS );
    createBooleanField( composite, LANG_FI );
    createBooleanField( composite, LANG_FFI );
    createBooleanField( composite, LANG_WITH );
    createBooleanField( composite, LANG_NO_MONOMORPHISM_RESTRICTION );
    createBooleanField( composite, LANG_ALLOW_OVERLAPPING_INSTANCES );
    createBooleanField( composite, LANG_ALLOW_UNDECIDABLE_INSTANCES );
    createBooleanField( composite, LANG_ALLOW_INCOHERENT_INSTANCES );
    createBooleanField( composite, LANG_GENERICS );
    createBooleanField( composite, LANG_NO_IMPLICIT_PRELUDE );
    
    return composite;
  }
}