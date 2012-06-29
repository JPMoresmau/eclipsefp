/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;


/**
 * Base class for all helper executable preference page
 * Allows choosing an exe or detecting it in the path
 * @author JP Moresmau
 *
 */
public abstract class ExecutablePP extends PreferencePage implements IWorkbenchPreferencePage{
  private AutodetectExecutableField executableField;

  private final String pgmName;
  private final String exeName;
  private final String pref;


  public ExecutablePP( final String pgmName, final String exeName, final String pref ) {
    super();
    this.pgmName = pgmName;
    this.exeName = exeName;
    this.pref = pref;
  }


  /* (non-Javadoc)
   * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( final Composite parentComposite ) {
    noDefaultAndApplyButton();
    IPreferenceStore prefStore = HaskellUIPlugin.getDefault().getPreferenceStore();
    setPreferenceStore(prefStore);
    parentComposite.setLayout( new GridLayout(3,false) );


    IPropertyChangeListener propertyListener=new IPropertyChangeListener() {

      @Override
      public void propertyChange( final PropertyChangeEvent arg0 ) {
        setValid( isValid() );
      }
    };
    executableField=new AutodetectExecutableField( this, parentComposite, pgmName, exeName, pref,propertyListener );
    new Label(parentComposite,SWT.NONE);
    setValid( isValid() );
    return parentComposite;
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.preference.PreferencePage#performOk()
   */
  @Override
  public boolean performOk() {
    executableField.store();
    return super.performOk();
  }

  @Override
  public void init( final IWorkbench workbench ) {
    // unused
  }
}
