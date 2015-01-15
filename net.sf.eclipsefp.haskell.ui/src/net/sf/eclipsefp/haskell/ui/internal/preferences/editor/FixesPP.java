/**
 *  Copyright (c) 2015 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.preferences.editor;

import net.sf.eclipsefp.common.ui.preferences.overlay.OverlayPreferenceStore;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageVersion;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageVersion.Restriction;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;


/**
 * Preferences applying on quick fixes
 * @author JP Moresmau
 *
 */
public class FixesPP extends AbstractEditorPP {
  private final Button[] buttons=new Button[CabalPackageVersion.Restriction.values().length];

  private Restriction res=Restriction.NONE;

  /**
   *
   */
  public FixesPP() {
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.ui.internal.preferences.editor.AbstractEditorPP#addPreferences(net.sf.eclipsefp.common.ui.preferences.overlay.OverlayPreferenceStore)
   */
  @Override
  protected void addPreferences( final OverlayPreferenceStore store ) {
   store.addStringKey( EDITOR_FIXES_PACKAGE_RESTRICTION );

  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( final Composite parent ) {
    Composite control = new Composite( parent, SWT.NONE );
    GridLayout layout = new GridLayout(1,false);
    control.setLayout( layout );
    String s=getPreferenceStore().getString( EDITOR_FIXES_PACKAGE_RESTRICTION );
    res=Restriction.valueOf( s );

    Label lText=new Label( control, SWT.NONE );
    lText.setText( UITexts.preferences_editor_fixes_version );
    GridData gdText=new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL);
    lText.setLayoutData( gdText );

    for (final Restriction r:Restriction.values()){
      Button b=new Button(control,SWT.RADIO);
      buttons[r.ordinal()]=b;
      b.setText( getButtonLabel( r ) );
      GridData gdButton=new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL);
      gdButton.horizontalIndent=20;
      b.setLayoutData( gdButton );
      b.addSelectionListener( new SelectionAdapter() {
        @Override
        public void widgetSelected(final org.eclipse.swt.events.SelectionEvent e) {
          res=r;
          getPreferenceStore().setValue( EDITOR_FIXES_PACKAGE_RESTRICTION, r.name() );
        }
      } );
    }

    buttons[res.ordinal()].setSelection( true );
    return control;
  }

  private String getButtonLabel(final Restriction r){
    switch( r ) {
      case MAJOR:
        return UITexts.cabalEditor_dependencyVersionMajor;
      case MAJOR_FROM_MINOR:
        return UITexts.cabalEditor_dependencyVersionCurrent;
      case MINOR:
        return UITexts.cabalEditor_dependencyVersionMinor;
      default:
        return UITexts.cabalEditor_dependencyVersionNone;
    }
  }
}
