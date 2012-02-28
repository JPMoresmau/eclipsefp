package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.SWTUtil;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * The Scion preferences page in the Preferences dialog.
 *
 * @author Thomas ten Cate
 * @author Alejandro Serrano (Browser integration)
 */

public class ScionPP
	extends PreferencePage
	implements IWorkbenchPreferencePage, IPreferenceConstants {

  public static final String PAGE_ID = ScionPP.class.getName();

	private AutodetectExecutableField buildWrapperExecutableField;

	private AutodetectExecutableField browserExecutableField;

  private BooleanFieldEditor browserUseHackage;

  private AutodetectExecutableField hoogleExecutableField;

  private AutodetectExecutableField hlintExecutableField;

	private BooleanFieldEditor verboseInteractionField;
  private Composite verboseInteractionFieldC;

  private BooleanFieldEditor verboseBrowserInteractionField;
  private Composite verboseBrowserInteractionFieldC;

	private CabalImplsBlock cabalBlock;

  private BooleanFieldEditor ignoreMissing;
  private BooleanFieldEditor ignoreTooOld;

	public ScionPP() {
	  super();
    setPreferenceStore(HaskellUIPlugin.getDefault().getPreferenceStore());
	}

	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	@Override
  protected Composite createContents( final Composite parentComposite ) {
	  final int nColumns = 3;

	  // FIXME: Need to add fields for console high and low water marks, hook preference changes to ScionManager

	  // Create the page:
	  noDefaultAndApplyButton();
	  IPreferenceStore prefStore = HaskellUIPlugin.getDefault().getPreferenceStore();
    setPreferenceStore(prefStore);
    parentComposite.setLayout( new GridLayout(nColumns,false) );

    IPropertyChangeListener propertyListener=new IPropertyChangeListener() {

      public void propertyChange( final PropertyChangeEvent arg0 ) {
        updateButtonState();
        setValid( isValid() );
      }
    };

	  SWTUtil.createMessageLabel( parentComposite, UITexts.scion_preferences_title, nColumns, SWT.DEFAULT );
	  SWTUtil.createLineSpacer( parentComposite, 1 );

    cabalBlock = new CabalImplsBlock();
    Control control = cabalBlock.createControl( parentComposite, this );
    cabalBlock.addSelectionChangedListener( new ISelectionChangedListener() {
      public void selectionChanged( final SelectionChangedEvent event ) {
        setValid( isValid() );
      }
    } );

    GridData gdata = new GridData( GridData.GRAB_HORIZONTAL | GridData.HORIZONTAL_ALIGN_FILL | GridData.VERTICAL_ALIGN_FILL);
    gdata.horizontalSpan = nColumns;
    control.setLayoutData( gdata );

    // Restore dialog settings for the page, if set. Currently, only the Cabal implementations
    // block actually stores its settings.
    IDialogSettings dlgSettings = HaskellUIPlugin.getDefault().getDialogSettings();
    cabalBlock.restoreColumnSettings( dlgSettings, PAGE_ID );

    Group bwComposite = new Group(parentComposite, SWT.NONE);
    bwComposite.setLayout( new GridLayout( 2, false ) );
    GridData gridData = new GridData( GridData.GRAB_HORIZONTAL | GridData.HORIZONTAL_ALIGN_FILL);
    gridData.horizontalSpan = nColumns;
    bwComposite.setLayoutData( gridData );
    bwComposite.setText( UITexts.buildwrapper_preferences_label );

    buildWrapperExecutableField=new AutodetectExecutableField( this, bwComposite, "BuildWrapper", "buildwrapper", IPreferenceConstants.BUILDWRAPPER_EXECUTABLE,propertyListener );



    verboseInteractionFieldC = new Composite(bwComposite, SWT.NONE);
    GridData gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL | GridData.VERTICAL_ALIGN_CENTER);
    gd.horizontalSpan=2;
    verboseInteractionFieldC.setLayoutData( gd);
    verboseInteractionField = new BooleanFieldEditor( IPreferenceConstants.VERBOSE_INTERACTION,
        UITexts.scionVerboseInteraction_title,
        verboseInteractionFieldC );
    verboseInteractionField.setPage(this);
    verboseInteractionField.setPreferenceStore( prefStore );
    verboseInteractionField.load();

    // scion-browser

		Group sbComposite = new Group(parentComposite, SWT.NONE);
		sbComposite.setLayout( new GridLayout( 2, false ) );
    gridData = new GridData( GridData.GRAB_HORIZONTAL | GridData.HORIZONTAL_ALIGN_FILL);
    gridData.horizontalSpan = nColumns;
    sbComposite.setLayoutData( gridData );
    sbComposite.setText( UITexts.scionBrowser_preferences_label );

    browserExecutableField=new AutodetectExecutableField( this, sbComposite, "Scion-Browser", "scion-browser", IPreferenceConstants.SCION_BROWSER_SERVER_EXECUTABLE,propertyListener );


    browserUseHackage = new BooleanFieldEditor( IPreferenceConstants.SCION_BROWSER_USE_HACKAGE, UITexts.scionBrowserUseHackage_label, sbComposite );
    browserUseHackage.setPage( this );
    browserUseHackage.setPreferenceStore( prefStore );
    browserUseHackage.load();

    verboseBrowserInteractionFieldC = new Composite(sbComposite, SWT.NONE);
    gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL | GridData.VERTICAL_ALIGN_CENTER);
    gd.horizontalSpan=2;
    verboseBrowserInteractionFieldC.setLayoutData( gd);
    verboseBrowserInteractionField = new BooleanFieldEditor( IPreferenceConstants.BROWSER_VERBOSE_INTERACTION,
        UITexts.browserVerboseInteraction_title,
        verboseBrowserInteractionFieldC );
    verboseBrowserInteractionField.setPage(this);
    verboseBrowserInteractionField.setPreferenceStore( prefStore );
    verboseBrowserInteractionField.load();

    hoogleExecutableField=new AutodetectExecutableField( this, sbComposite, "Hoogle", "hoogle", IPreferenceConstants.SCION_BROWSER_EXTRA_HOOGLE_PATH,propertyListener );

    Group hlintComposite = new Group(parentComposite, SWT.NONE);
    hlintComposite.setLayout( new GridLayout( 2, false ) );
    gridData = new GridData( GridData.GRAB_HORIZONTAL | GridData.HORIZONTAL_ALIGN_FILL);
    gridData.horizontalSpan = nColumns;
    hlintComposite.setLayoutData( gridData );
    hlintComposite.setText( UITexts.hlint_preferences_label );
    hlintExecutableField=new AutodetectExecutableField( this, hlintComposite, "HLint", "hlint", IPreferenceConstants.HLINT_EXECUTABLE,propertyListener );


    ignoreMissing=new BooleanFieldEditor( IPreferenceConstants.IGNORE_MISSING_EXECUTABLE, UITexts.ignore_missing_button, parentComposite );
    ignoreMissing.setPage( this );
    ignoreMissing.setPreferenceStore( prefStore );
    ignoreMissing.load();

    ignoreTooOld=new BooleanFieldEditor( IPreferenceConstants.IGNORE_TOOOLD_EXECUTABLE, UITexts.ignore_tooold_button, parentComposite );
    ignoreTooOld.setPage( this );
    ignoreTooOld.setPreferenceStore( prefStore );
    ignoreTooOld.load();

		// Update the dialog's state and validity:
		updateButtonState();
		setValid(isValid());

		return parentComposite;
	}



	private void updateButtonState() {

    buildWrapperExecutableField.setEnabled( true );
    browserExecutableField.setEnabled( true );
    hoogleExecutableField.setEnabled( true );
    hlintExecutableField.setEnabled( true );
	}


	public static void initializeDefaults(final IPreferenceStore store) {
	  store.setDefault( BUILDWRAPPER_EXECUTABLE, new String() );
	  store.setDefault( SCION_BROWSER_SERVER_EXECUTABLE, new String() );
	  store.setDefault( SCION_BROWSER_USE_HACKAGE, false );
	  store.setDefault( VERBOSE_INTERACTION, false );
	  store.setDefault( BROWSER_VERBOSE_INTERACTION, false );
	  store.setDefault( SCION_BROWSER_EXTRA_HOOGLE_PATH, "" );
	}

  @Override
  public boolean performOk() {
    cabalBlock.updateCabalImplementations();
    buildWrapperExecutableField.store();
    browserExecutableField.store();
    hoogleExecutableField.store();
    browserUseHackage.store();
    ignoreMissing.store();
    ignoreTooOld.store();
    verboseInteractionField.store();
    verboseBrowserInteractionField.store();

    hlintExecutableField.store();

    IDialogSettings settings = HaskellUIPlugin.getDefault().getDialogSettings();
    cabalBlock.saveColumnSettings( settings, PAGE_ID );

    if (super.performOk()) {
      // Yuck. You'd think there'd be a way to do this via listening for preference
      // changes, but nooooooh.
      //HaskellUIPlugin.getDefault().getScionManager().handlePreferenceChanges();
      return true;
    } else {
      return false;
    }
  }

  @Override
  public boolean performCancel() {
    return super.performCancel();
  }

  @Override
  public boolean isValid() {
    boolean retval = cabalBlock.validate( this );
    if (retval) {
      //if (!serverBuiltInField.getBooleanValue()) {
        if (!buildWrapperExecutableField.isValid()) {
          retval = false;
          setErrorMessage( UITexts.cabalImplsBlock_needBuildWrapperExecutablePath );
        }
      //}
    }

    if (retval) {
      //if (!browserBuiltInField.getBooleanValue()) {
        if (!browserExecutableField.isValid()) {
          retval = false;
          setErrorMessage( UITexts.cabalImplsBlock_needScionExecutablePath );
        }
      //}
    }

    if (retval) {
      // Clear the message
      setMessage( null );
      setErrorMessage( null );
    }

    return retval;
  }

	@Override
	public void dispose() {
	  // unused
	}

  public void init( final IWorkbench workbench ) {
    // unused
  }
}
