// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.preferences.hsimpls;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.compiler.IHsImplementation;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.SWTUtil;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.osgi.service.prefs.BackingStoreException;


/** <p>the preference page for installed Haskell implementations. A Haskell
  * implementation (i.e. a compiler or interpreter) can be selected to
  * be the currently used one in the workspace. Haskell implementations
  * can be added and removed.</p>
  *
  * <p>This is mostly analoguous to the 'Installed JREs' page in the JDT
  * Java IDE.</p>
  *
  * @author Leif Frenzel
  */
public class InstalledImplementationsPP extends PreferencePage
                                        implements IWorkbenchPreferencePage {

  private static final String DIALOG_SETTINGS_ID
    = InstalledImplementationsPP.class.getName();

  // interface methods of PreferencePage
  //////////////////////////////////////

  private ImplementationsBlock implementationsBlock;

  @Override
  protected Control createContents( final Composite parent ) {
    initializeDialogUnits( parent );
    noDefaultAndApplyButton();

    GridLayout layout = new GridLayout( 1, false );
    layout.marginHeight = 0;
    layout.marginWidth = 0;
    parent.setLayout( layout );

    SWTUtil.createMessageLabel( parent, UITexts.installedImplementationsPP_msg, 1, 300 );
    SWTUtil.createLineSpacer( parent, 1 );

    implementationsBlock = new ImplementationsBlock();
    Control control = implementationsBlock.createControl( parent );
    GridData data = new GridData( GridData.FILL_BOTH );
    data.horizontalSpan = 1;
    control.setLayoutData( data );
    IDialogSettings dlgSettings = HaskellUIPlugin.getDefault().getDialogSettings();
    implementationsBlock.restoreColumnSettings( dlgSettings, DIALOG_SETTINGS_ID );

    applyInitialValue();
    initLogic();
    applyDialogFont( parent );
    return parent;
  }

  @Override
  public boolean performOk() {
    IEclipsePreferences node = HaskellCorePlugin.instanceScopedPreferences();

    node.put( ICorePreferenceNames.HS_IMPLEMENTATIONS, implementationsBlock.getPref() );
    IHsImplementation impl = implementationsBlock.getCheckedHsImplementation();
    String name = ""; //$NON-NLS-1$
    if( impl != null ) {
      name = impl.getName();
    }
    node.put( ICorePreferenceNames.SELECTED_HS_IMPLEMENTATION, name );

    try {
      node.flush();
    } catch( BackingStoreException ex ) {
      HaskellUIPlugin.log( ex );
    }

    IDialogSettings settings = HaskellUIPlugin.getDefault().getDialogSettings();
    implementationsBlock.saveColumnSettings( settings, DIALOG_SETTINGS_ID );
    return true;
  }


  // interface methods of IWorkbenchPreferencePage
  ////////////////////////////////////////////////

  public void init( final IWorkbench workbench ) {
    // unused
  }


  // helping functions
  ////////////////////

  private void initLogic() {
    ISelectionChangedListener listener = new ISelectionChangedListener() {
      public void selectionChanged( final SelectionChangedEvent event ) {
        IHsImplementation install = implementationsBlock.getCheckedHsImplementation();
        if( install == null ) {
          setValid( false );
          setErrorMessage( UITexts.installedImplementationsPP_nothingSelected );
        } else {
          setValid( true );
          setMessage( null );
          setErrorMessage( null );
        }
      }
    };
    implementationsBlock.addSelectionChangedListener( listener );
  }

  private void applyInitialValue() {
    String impls = Platform.getPreferencesService().getString( HaskellCorePlugin.getPluginId(), ICorePreferenceNames.HS_IMPLEMENTATIONS, null, null );
    implementationsBlock.applyPref( impls );

    String sel = Platform.getPreferencesService().getString( HaskellCorePlugin.getPluginId(), ICorePreferenceNames.SELECTED_HS_IMPLEMENTATION, null, null );
    implementationsBlock.setCheckedHsImplementation( sel );
  }

}