package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import java.io.File;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.SWTUtil;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
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

	private ExecutableFileFieldEditor serverExecutableField;
	private Composite serverExecutableFieldC;
//	private BooleanFieldEditor serverBuiltInField;
//	private Composite serverBuiltInFieldC;
	private ButtonFieldEditor autodetect;
	private Composite autodetectC;
//	private Button forceRebuild;

	private ExecutableFileFieldEditor browserExecutableField;
  private Composite browserExecutableFieldC;
//	private BooleanFieldEditor browserBuiltInField;
//  private Composite browserBuiltInFieldC;
  private ButtonFieldEditor autodetectBrowser;
  private Composite autodetectBrowserC;
//  private Button forceRebuildBrowser;
  private BooleanFieldEditor browserUseHackage;

  private ExecutableFileFieldEditor hoogleExecutableField;
  private Composite hoogleExecutableFieldC;

//	private BooleanFieldEditor cabalUpdateField;
	//private Composite forceRebuildC;
//	private RadioGroupFieldEditor serverFlavorField;
//	private Composite serverFlavorFieldC;
	private BooleanFieldEditor verboseInteractionField;
  private Composite verboseInteractionFieldC;

	private CabalImplsBlock cabalBlock;
	//private Group fieldComposite;
//	private boolean rebuildBuiltin;
//	private boolean rebuildBrowserBuiltin;

	public ScionPP() {
	  super();
    setPreferenceStore(HaskellUIPlugin.getDefault().getPreferenceStore());
//    rebuildBuiltin = false;
//    rebuildBrowserBuiltin = false;
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

	  // Member variable initialization:
//	  rebuildBuiltin = false;
//	  rebuildBrowserBuiltin = false;

	  // FIXME: Need to add fields for server verbosity (do we really want to watch server interaction messages?)
	  // FIXME: Need to add fields for console high and low water marks, hook preference changes to ScionManager

	  // Create the page:
	  noDefaultAndApplyButton();
	  IPreferenceStore prefStore = HaskellUIPlugin.getDefault().getPreferenceStore();
	 //IPreferenceStore scionPrefStore = ScionPlugin.getDefault().getPreferenceStore();
    setPreferenceStore(prefStore);
    parentComposite.setLayout( new GridLayout(nColumns,false) );

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
    //SWTUtil.createMessageLabel (fieldComposite, UITexts.buildwrapper_preferences_label, 2, SWT.DEFAULT);

//    serverBuiltInFieldC=new Composite(fieldComposite,SWT.NONE);
//    gdata = new GridData( SWT.FILL, SWT.CENTER, true, false );
//    gdata.horizontalSpan=2;
//    serverBuiltInFieldC.setLayoutData( gdata );
//		serverBuiltInField = new BooleanFieldEditor( IPreferenceConstants.SCION_SERVER_BUILTIN,
//		                                             UITexts.scionServerBuiltIn_label,
//		                                             serverBuiltInFieldC);
//		serverBuiltInField.setPropertyChangeListener( new IPropertyChangeListener() {
//      public void propertyChange( final PropertyChangeEvent event ) {
//        updateButtonState();
//        setValid( isValid() );
//      }
//    } );
//		serverBuiltInField.setPage( this );
//		serverBuiltInField.setPreferenceStore( prefStore );
//		serverBuiltInField.load();

		//forceRebuildC=new Composite(fieldComposite,SWT.NONE);
//    gdata = new GridData( SWT.LEFT, SWT.CENTER, true, false );
//    gdata.horizontalIndent=30;
//    gdata.horizontalSpan=1;
//    //forceRebuildC.setLayoutData( gdata );
//    forceRebuild=new Button(fieldComposite,SWT.CHECK);
//    forceRebuild.setText( UITexts.forceRebuildButton_text );
//    forceRebuild.setLayoutData( gdata );


    /*forceRebuild = new ButtonFieldEditor(
        UITexts.forceRebuildButton_text,
        UITexts.forceRebuildButton_label,
        new SelectionAdapter() {
          @Override
          public void widgetSelected(final SelectionEvent e) {
            rebuildBuiltin = !rebuildBuiltin;
          }
        },
        forceRebuildC ){
      @Override
      protected int getButtonStyle() {
       return SWT.CHECK;
      }
    };
    forceRebuild.setPage( this );
    forceRebuild.setPreferenceStore( prefStore );
    forceRebuild.load();*/

//    final Composite cabalUpdateFieldC=new Composite(fieldComposite,SWT.NONE);
//    gdata = new GridData( SWT.FILL, SWT.CENTER, true, false );
//    cabalUpdateFieldC.setLayoutData( gdata );
//    cabalUpdateField=new BooleanFieldEditor( IPreferenceConstants.RUN_CABAL_UPDATE, UITexts.cabalUpdateButton_text, cabalUpdateFieldC );
//    cabalUpdateField.setPage( this );
//    cabalUpdateField.setPreferenceStore( getPreferenceStore() );
//    cabalUpdateField.load();
//    cabalUpdateField.setEnabled( false, cabalUpdateFieldC );
//
//    forceRebuild.addSelectionListener( new SelectionAdapter() {
//      @Override
//      public void widgetSelected( final SelectionEvent e ) {
//        rebuildBuiltin=forceRebuild.getSelection();
//        cabalUpdateField.setEnabled( rebuildBuiltin, cabalUpdateFieldC );
//      }
//    });

    serverExecutableFieldC=new Composite(bwComposite,SWT.NONE);
    GridData gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL | GridData.VERTICAL_ALIGN_CENTER);
    gd.horizontalSpan=2;
    serverExecutableFieldC.setLayoutData( gd );
		serverExecutableField = new ExecutableFileFieldEditor(IPreferenceConstants.BUILDWRAPPER_EXECUTABLE,
				NLS.bind(UITexts.buildwrapperExecutable_label, getServerExecutableName()),
				false, StringFieldEditor.VALIDATE_ON_KEY_STROKE, serverExecutableFieldC );
		serverExecutableField.setEmptyStringAllowed(true);
		serverExecutableField.setPropertyChangeListener( new IPropertyChangeListener() {
      public void propertyChange( final PropertyChangeEvent event ) {
        setValid( isValid() );
      }
    });
		serverExecutableField.setPage( this );
		serverExecutableField.setPreferenceStore( prefStore );
		serverExecutableField.load();

		autodetectC=new Composite(bwComposite,SWT.NONE);
    gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL | GridData.VERTICAL_ALIGN_CENTER);
    gd.horizontalSpan=2;
    autodetectC.setLayoutData( gd );
		autodetect = new ButtonFieldEditor(
				String.format(UITexts.autodetectButton_label, getServerExecutableName()),
				UITexts.autodetectButton_text,
				new SelectionAdapter() {
					@Override
					public void widgetSelected(final SelectionEvent e) {
						doDetectServer();
						updateButtonState();
						setValid( isValid() );
					}
				},
				autodetectC );
		autodetect.setPage( this );
		autodetect.setPreferenceStore( prefStore );
		autodetect.load();

//		serverFlavorFieldC = new Composite(fieldComposite, SWT.NONE);
//    gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL | GridData.VERTICAL_ALIGN_CENTER);
//    gd.horizontalSpan=2;
//    serverFlavorFieldC.setLayoutData( gd);
//		serverFlavorField = new RadioGroupFieldEditor( IPreferenceConstants.SCION_SERVER_FLAVOR,
//		    UITexts.scionServerFlavor_title, 1,
//		    new String[][] {
//		      { UITexts.scionServerFlavor_stdstream_label, ScionManager.STDSTREAM_SCION_FLAVOR },
//		      { UITexts.scionServerFlavor_network_label, ScionManager.NETWORK_SCION_FLAVOR }
//		    },
//		    serverFlavorFieldC, true );
//		serverFlavorField.setPage(this);
//		serverFlavorField.setPreferenceStore( prefStore );
//		serverFlavorField.load();

    verboseInteractionFieldC = new Composite(bwComposite, SWT.NONE);
    gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL | GridData.VERTICAL_ALIGN_CENTER);
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

    //SWTUtil.createMessageLabel (bwComposite, UITexts.scionBrowser_preferences_label, 2, SWT.DEFAULT);

//    browserBuiltInFieldC=new Composite(sbComposite,SWT.NONE);
//    gdata = new GridData( SWT.FILL, SWT.CENTER, true, false );
//    gdata.horizontalSpan=2;
//    browserBuiltInFieldC.setLayoutData( gdata );
//    browserBuiltInField = new BooleanFieldEditor( IPreferenceConstants.SCION_BROWSER_SERVER_BUILTIN,
//                                                  UITexts.scionBrowserBuiltIn_label,
//                                                  browserBuiltInFieldC);
//    browserBuiltInField.setPropertyChangeListener( new IPropertyChangeListener() {
//      public void propertyChange( final PropertyChangeEvent event ) {
//        updateButtonState();
//        setValid( isValid() );
//      }
//    } );
//    browserBuiltInField.setPage( this );
//    browserBuiltInField.setPreferenceStore( prefStore );
//    browserBuiltInField.load();

    //forceRebuildC=new Composite(fieldComposite,SWT.NONE);
//    gdata = new GridData( SWT.LEFT, SWT.CENTER, true, false );
//    gdata.horizontalIndent=30;
//    gdata.horizontalSpan=1;
//    //forceRebuildC.setLayoutData( gdata );
//    forceRebuildBrowser=new Button(sbComposite,SWT.CHECK);
//    forceRebuildBrowser.setText( UITexts.forceRebuildBrowserButton_text );
//    forceRebuildBrowser.setLayoutData( gdata );
//
//    forceRebuildBrowser.addSelectionListener( new SelectionAdapter() {
//      @Override
//      public void widgetSelected( final SelectionEvent e ) {
//        rebuildBrowserBuiltin=forceRebuildBrowser.getSelection();
//       // cabalUpdateField.setEnabled( rebuildBrowserBuiltin, cabalUpdateFieldC );
//      }
//    });

    browserExecutableFieldC=new Composite(sbComposite,SWT.NONE);
    GridData gd2 = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL | GridData.VERTICAL_ALIGN_CENTER);
    gd2.horizontalSpan=2;
    browserExecutableFieldC.setLayoutData( gd2 );
    browserExecutableField = new ExecutableFileFieldEditor(IPreferenceConstants.SCION_BROWSER_SERVER_EXECUTABLE,
        NLS.bind(UITexts.scionBrowserExecutable_label, getBrowserExecutableName()),
        false, StringFieldEditor.VALIDATE_ON_KEY_STROKE, browserExecutableFieldC );
    browserExecutableField.setEmptyStringAllowed(true);
    browserExecutableField.setPropertyChangeListener( new IPropertyChangeListener() {
      public void propertyChange( final PropertyChangeEvent event ) {
        setValid( isValid() );
      }
    });
    browserExecutableField.setPage( this );
    browserExecutableField.setPreferenceStore( prefStore );
    browserExecutableField.load();

    autodetectBrowserC=new Composite(sbComposite,SWT.NONE);
    gd2 = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL | GridData.VERTICAL_ALIGN_CENTER);
    gd2.horizontalSpan=2;
    autodetectBrowserC.setLayoutData( gd2 );
    autodetectBrowser = new ButtonFieldEditor(
        String.format(UITexts.autodetectBrowserButton_label, getBrowserExecutableName()),
        UITexts.autodetectButton_text,
        new SelectionAdapter() {
          @Override
          public void widgetSelected(final SelectionEvent e) {
            doDetectBrowser();
            updateButtonState();
            setValid( isValid() );
          }
        },
        autodetectBrowserC );
    autodetectBrowser.setPage( this );
    autodetectBrowser.setPreferenceStore( prefStore );
    autodetectBrowser.load();

    browserUseHackage = new BooleanFieldEditor( IPreferenceConstants.SCION_BROWSER_USE_HACKAGE, UITexts.scionBrowserUseHackage_label, sbComposite );
    browserUseHackage.setPage( this );
    browserUseHackage.setPreferenceStore( prefStore );
    browserUseHackage.load();

    hoogleExecutableFieldC=new Composite(sbComposite,SWT.NONE);
    GridData gd3 = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL | GridData.VERTICAL_ALIGN_CENTER);
    gd3.horizontalSpan=2;
    hoogleExecutableFieldC.setLayoutData( gd3 );
    hoogleExecutableField = new ExecutableFileFieldEditor(IPreferenceConstants.SCION_BROWSER_EXTRA_HOOGLE_PATH,
        NLS.bind(UITexts.scionBrowserExecutable_label, getHoogleExecutableName()),
        false, StringFieldEditor.VALIDATE_ON_KEY_STROKE, hoogleExecutableFieldC );
    hoogleExecutableField.setEmptyStringAllowed(true);
    /* hoogleExecutableField.setPropertyChangeListener( new IPropertyChangeListener() {
      public void propertyChange( final PropertyChangeEvent event ) {
        setValid( isValid() );
      }
    }); */
    hoogleExecutableField.setPage( this );
    hoogleExecutableField.setPreferenceStore( prefStore );
    hoogleExecutableField.load();

		// Update the dialog's state and validity:
		updateButtonState();
		setValid(isValid());

		return parentComposite;
	}

	private void doDetectServer() {
		String server = detectScionServer();
		if (server == null) {
			MessageDialog.openError(getShell(),
					UITexts.autodetectButton_errorTitle,
					NLS.bind(UITexts.autodetectButton_errorMessage, getServerExecutableName()));
		} else {
			serverExecutableField.setStringValue(server);
		}
	}

	private void doDetectBrowser() {
    String server = detectBrowserServer();
    if (server == null) {
      MessageDialog.openError(getShell(),
          UITexts.autodetectBrowserButton_errorTitle,
          NLS.bind(UITexts.autodetectBrowserButton_errorMessage, getBrowserExecutableName()));
    } else {
      browserExecutableField.setStringValue(server);
    }
  }

	private void updateButtonState() {
    boolean b = false; //serverBuiltInField.getBooleanValue();
    //forceRebuild.setEnabled( b, forceRebuildC );
//    forceRebuild.setEnabled( b);
//    if (!b){
//      forceRebuild.setSelection( false );
//      forceRebuild.notifyListeners( SWT.Selection, new Event() );
//    }
    autodetect.setEnabled( !b, autodetectC );
    serverExecutableField.setEnabled( !b, serverExecutableFieldC );

    boolean b2 =false; //browserBuiltInField.getBooleanValue();
    //forceRebuild.setEnabled( b, forceRebuildC );
//    forceRebuildBrowser.setEnabled( b2 );
//    if (!b2){
//      forceRebuildBrowser.setSelection( false );
//      forceRebuildBrowser.notifyListeners( SWT.Selection, new Event() );
//    }
    autodetectBrowser.setEnabled( !b2, autodetectBrowserC );
    browserExecutableField.setEnabled( !b2, browserExecutableFieldC );
	}

	/**
	 * Attempts to autodetect the path to the Scion server executable.
	 *
	 * @return the filename of the Scion server, or null if it could not be found
	 */
	private String detectScionServer() {
	  File f=FileUtil.findExecutableInPath( getServerExecutableName() );
	  return f!=null?f.getAbsolutePath():null;
	}

	private String detectBrowserServer() {
    File f=FileUtil.findExecutableInPath( getBrowserExecutableName() );
    return f!=null?f.getAbsolutePath():null;
  }

	public static String getServerExecutableName() {
		return FileUtil.makeExecutableName("buildwrapper"); //$NON-NLS-1$
	}

	public static String getBrowserExecutableName() {
    return FileUtil.makeExecutableName("scion-browser"); //$NON-NLS-1$
  }

	public static String getHoogleExecutableName() {
    return FileUtil.makeExecutableName("hoogle"); //$NON-NLS-1$
  }

	public static void initializeDefaults(final IPreferenceStore store) {
	  // Set reasonable defaults.
	 // store.setDefault( SCION_SERVER_BUILTIN, true );
	 // store.setDefault( SCION_BROWSER_SERVER_BUILTIN, true );
   // store.setDefault( RUN_CABAL_UPDATE, true );
	  store.setDefault( BUILDWRAPPER_EXECUTABLE, new String() );
	  store.setDefault( SCION_BROWSER_SERVER_EXECUTABLE, new String() );
	  store.setDefault( SCION_BROWSER_USE_HACKAGE, false );
	  store.setDefault( VERBOSE_INTERACTION, false );
	  store.setDefault( SCION_BROWSER_EXTRA_HOOGLE_PATH, "" );
//	  store.setDefault( IScionPreferenceNames.VERBOSE_INTERACTION, false );
	}

  @Override
  public boolean performOk() {
    cabalBlock.updateCabalImplementations();
 //   serverBuiltInField.store();
  //  browserBuiltInField.store();
    serverExecutableField.store();
    browserExecutableField.store();
    hoogleExecutableField.store();
    autodetect.store();
    autodetectBrowser.store();
    browserUseHackage.store();
//    serverFlavorField.store();
   verboseInteractionField.store();
//    cabalUpdateField.store();

    IDialogSettings settings = HaskellUIPlugin.getDefault().getDialogSettings();
    cabalBlock.saveColumnSettings( settings, PAGE_ID );

    if (super.performOk()) {
      // Yuck. You'd think there'd be a way to do this via listening for preference
      // changes, but nooooooh.
      HaskellUIPlugin.getDefault().getScionManager().handlePreferenceChanges();
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
        if (serverExecutableField.getStringValue().length() == 0) {
          retval = false;
          setErrorMessage( UITexts.cabalImplsBlock_needBuildWrapperExecutablePath );
        }
      //}
    }

    if (retval) {
      //if (!browserBuiltInField.getBooleanValue()) {
        if (browserExecutableField.getStringValue().length() == 0) {
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
