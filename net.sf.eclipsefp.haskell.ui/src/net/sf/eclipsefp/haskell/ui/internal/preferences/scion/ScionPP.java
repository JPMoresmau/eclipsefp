package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.SWTUtil;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.forms.events.ExpansionAdapter;
import org.eclipse.ui.forms.events.ExpansionEvent;
import org.eclipse.ui.forms.widgets.ExpandableComposite;

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

  //private AutodetectExecutableField hlintExecutableField;

	private BooleanFieldEditor verboseInteractionField;

  private IntegerFieldEditor maxConfigureFailuresField;
  private Composite maxConfigureFailuresFieldC;
  private IntegerFieldEditor evalMaxField;
  private Composite evalMaxFieldFieldC;

  private BooleanFieldEditor verboseBrowserInteractionField;

  private BooleanFieldEditor browserStartPerspectiveField;

  private CabalImplsBlock cabalBlock;

  private BooleanFieldEditor ignoreMissing;
  private BooleanFieldEditor ignoreTooOld;

  private IntegerFieldEditor consoleMaxField;
  private BooleanFieldEditor consoleActivateField;

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

      @Override
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
      @Override
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

    maxConfigureFailuresFieldC = new Composite(bwComposite, SWT.NONE);
    GridData gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL | GridData.VERTICAL_ALIGN_CENTER);
    gd.horizontalSpan=2;
    maxConfigureFailuresFieldC.setLayoutData( gd);
    maxConfigureFailuresField = new IntegerFieldEditor( IPreferenceConstants.MAX_CONFIGURE_FAILURES,
        UITexts.maxConfigureFailures_title,
        maxConfigureFailuresFieldC );
    maxConfigureFailuresField.setValidRange( -1, Integer.MAX_VALUE );
    maxConfigureFailuresField.setPage(this);
    maxConfigureFailuresField.setPreferenceStore( prefStore );
    maxConfigureFailuresField.load();

    evalMaxFieldFieldC = new Composite(bwComposite, SWT.NONE);
    gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL | GridData.VERTICAL_ALIGN_CENTER);
    gd.horizontalSpan=2;
    evalMaxFieldFieldC.setLayoutData( gd);
    evalMaxField= new IntegerFieldEditor( IPreferenceConstants.MAX_EVAL_TIME,
        UITexts.maxEvalTime_title,
        evalMaxFieldFieldC );
    evalMaxField.setValidRange( 0, Integer.MAX_VALUE );
    evalMaxField.setPage(this);
    evalMaxField.setPreferenceStore( prefStore );
    evalMaxField.load();

    // scion-browser

		Group sbComposite = new Group(parentComposite, SWT.NONE);
		sbComposite.setLayout( new GridLayout( 2, false ) );
    gridData = new GridData( GridData.GRAB_HORIZONTAL | GridData.HORIZONTAL_ALIGN_FILL);
    gridData.horizontalSpan = nColumns;
    sbComposite.setLayoutData( gridData );
    sbComposite.setText( UITexts.scionBrowser_preferences_label );

    browserExecutableField=new AutodetectExecutableField( this, sbComposite, "Browser", "scion-browser", IPreferenceConstants.SCION_BROWSER_SERVER_EXECUTABLE,propertyListener );


    browserUseHackage = new BooleanFieldEditor( IPreferenceConstants.SCION_BROWSER_USE_HACKAGE, UITexts.scionBrowserUseHackage_label, sbComposite );
    browserUseHackage.setPage( this );
    browserUseHackage.setPreferenceStore( prefStore );
    browserUseHackage.load();



    hoogleExecutableField=new AutodetectExecutableField( this, sbComposite, "Hoogle", "hoogle", IPreferenceConstants.SCION_BROWSER_EXTRA_HOOGLE_PATH,propertyListener );

//    Group hlintComposite = new Group(parentComposite, SWT.NONE);
//    hlintComposite.setLayout( new GridLayout( 2, false ) );
//    gridData = new GridData( GridData.GRAB_HORIZONTAL | GridData.HORIZONTAL_ALIGN_FILL);
//    gridData.horizontalSpan = nColumns;
//    hlintComposite.setLayoutData( gridData );
//    hlintComposite.setText( UITexts.hlint_preferences_label );
//    hlintExecutableField=new AutodetectExecutableField( this, hlintComposite, "HLint", "hlint", IPreferenceConstants.HLINT_EXECUTABLE,propertyListener );

    ExpandableComposite advancedExpC=new ExpandableComposite( parentComposite, ExpandableComposite.TWISTIE | ExpandableComposite.CLIENT_INDENT );
    advancedExpC.setText( UITexts.executables_preferences_advanced );
    advancedExpC.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL | GridData.VERTICAL_ALIGN_CENTER) );

    Composite advancedC=new Composite(advancedExpC,SWT.NONE);
    advancedC.setLayout( new GridLayout(1,false) );
    advancedExpC.setClient( advancedC );
    advancedExpC.addExpansionListener(new ExpansionAdapter() {
      @Override
      public void expansionStateChanged(final ExpansionEvent e) {
        parentComposite.layout( true );
        ((ScrolledComposite)parentComposite.getParent().getParent()).setMinHeight( parentComposite.getParent().computeSize( SWT.DEFAULT, SWT.DEFAULT ).y );
      }
    });

    verboseInteractionField = new BooleanFieldEditor( IPreferenceConstants.VERBOSE_INTERACTION,
        UITexts.scionVerboseInteraction_title,
        advancedC );
    verboseInteractionField.setPage(this);
    verboseInteractionField.setPreferenceStore( prefStore );
    verboseInteractionField.load();


    verboseBrowserInteractionField = new BooleanFieldEditor( IPreferenceConstants.BROWSER_VERBOSE_INTERACTION,
        UITexts.browserVerboseInteraction_title,
        advancedC );
    verboseBrowserInteractionField.setPage(this);
    verboseBrowserInteractionField.setPreferenceStore( prefStore );
    verboseBrowserInteractionField.load();

    browserStartPerspectiveField= new BooleanFieldEditor( IPreferenceConstants.BROWSER_START_ONLY_PERSPECTIVE,
        UITexts.executables_preferences_browser_perspective,
        advancedC );
    browserStartPerspectiveField.setPage(this);
    browserStartPerspectiveField.setPreferenceStore( prefStore );
    browserStartPerspectiveField.load();

    ignoreMissing=new BooleanFieldEditor( IPreferenceConstants.IGNORE_MISSING_EXECUTABLE, UITexts.ignore_missing_button, advancedC );
    ignoreMissing.setPage( this );
    ignoreMissing.setPreferenceStore( prefStore );
    ignoreMissing.load();

    ignoreTooOld=new BooleanFieldEditor( IPreferenceConstants.IGNORE_TOOOLD_EXECUTABLE, UITexts.ignore_tooold_button, advancedC );
    ignoreTooOld.setPage( this );
    ignoreTooOld.setPreferenceStore( prefStore );
    ignoreTooOld.load();

    Composite consoleMaxFieldC=new Composite(advancedC,SWT.NONE);
    consoleMaxFieldC.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL | GridData.VERTICAL_ALIGN_CENTER) );
    consoleMaxField=new IntegerFieldEditor( IPreferenceConstants.HASKELL_CONSOLE_HIGH_WATER_MARK, UITexts.executables_preferences_console_high, consoleMaxFieldC );
    consoleMaxField.setPage( this );
    consoleMaxField.setPreferenceStore( prefStore );
    consoleMaxField.load();

    consoleActivateField=new BooleanFieldEditor( IPreferenceConstants.HASKELL_CONSOLE_ACTIVATE_ON_WRITE, UITexts.executables_preferences_console_activateonwrite, advancedC );
    consoleActivateField.setPage( this );
    consoleActivateField.setPreferenceStore( prefStore );
    consoleActivateField.load();

		// Update the dialog's state and validity:
		updateButtonState();
		setValid(isValid());

		return parentComposite;
	}



	private void updateButtonState() {

    buildWrapperExecutableField.setEnabled( true );
    browserExecutableField.setEnabled( true );
    hoogleExecutableField.setEnabled( true );
  //  hlintExecutableField.setEnabled( true );
	}


	public static void initializeDefaults(final IPreferenceStore store) {
	  store.setDefault( BUILDWRAPPER_EXECUTABLE, new String() );
	  store.setDefault( SCION_BROWSER_SERVER_EXECUTABLE, new String() );
	  store.setDefault( SCION_BROWSER_USE_HACKAGE, false );
	  store.setDefault( VERBOSE_INTERACTION, false );
	  store.setDefault( BROWSER_VERBOSE_INTERACTION, false );
	  store.setDefault( SCION_BROWSER_EXTRA_HOOGLE_PATH, "" );
	  store.setDefault( BROWSER_START_ONLY_PERSPECTIVE, true );
	  store.setDefault( MAX_CONFIGURE_FAILURES, 10 );
	  store.setDefault( MAX_EVAL_TIME, 30 );
	  store.setDefault( IPreferenceConstants.HASKELL_CONSOLE_HIGH_WATER_MARK, 32 * 1024);// 32K
	  store.setDefault( IPreferenceConstants.HASKELL_CONSOLE_ACTIVATE_ON_WRITE,false);
	  store.setDefault( UNIQUE_SANDBOX, false );
	  store.setDefault( UNIQUE_SANDBOX_PATH, BuildWrapperPlugin.getDefaultUniqueCabalSandboxLocation().toOSString() );
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
    maxConfigureFailuresField.store();
    verboseBrowserInteractionField.store();
    browserStartPerspectiveField.store();
    consoleActivateField.store();
    consoleMaxField.store();

 //   hlintExecutableField.store();

    IDialogSettings settings = HaskellUIPlugin.getDefault().getDialogSettings();
    cabalBlock.saveColumnSettings( settings, PAGE_ID );

    return super.performOk();
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

    /** do not force scion-browser to be present **/
//    if (retval) {
//      //if (!browserBuiltInField.getBooleanValue()) {
//        if (!browserExecutableField.isValid()) {
//          retval = false;
//          setErrorMessage( UITexts.cabalImplsBlock_needBrowserExecutablePath );
//        }
//      //}
//    }

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

  @Override
  public void init( final IWorkbench workbench ) {
    // unused
  }
}
