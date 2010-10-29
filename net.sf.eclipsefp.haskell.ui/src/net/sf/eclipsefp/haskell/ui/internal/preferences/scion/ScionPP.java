package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import java.io.File;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.scion.ScionManager;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.SWTUtil;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
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
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * The Scion preferences page in the Preferences dialog.
 *
 * @author Thomas ten Cate
 */

public class ScionPP
	extends PreferencePage
	implements IWorkbenchPreferencePage, IPreferenceConstants {

  public static final String PAGE_ID = ScionPP.class.getName();

	private ExecutableFileFieldEditor serverExecutableField;
	private Composite serverExecutableFieldC;
	private BooleanFieldEditor serverBuiltInField;
	private Composite serverBuiltInFieldC;
	private ButtonFieldEditor autodetect;
	private Composite autodetectC;
	private ButtonFieldEditor forceRebuild;
	private Composite forceRebuildC;
	private RadioGroupFieldEditor serverFlavorField;
	private Composite serverFlavorFieldC;

	private CabalImplsBlock cabalBlock;
	private Composite fieldComposite;
	private boolean rebuildBuiltin;

	public ScionPP() {
	  super();
    setPreferenceStore(HaskellUIPlugin.getDefault().getPreferenceStore());
    rebuildBuiltin = false;
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
	  rebuildBuiltin = false;

	  // Create the page:
	  noDefaultAndApplyButton();
	  IPreferenceStore prefStore = HaskellUIPlugin.getDefault().getPreferenceStore();
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

    fieldComposite = new Composite(parentComposite, SWT.NONE);
    fieldComposite.setLayout( new GridLayout( 2, false ) );
    GridData gridData = new GridData( GridData.GRAB_HORIZONTAL | GridData.HORIZONTAL_ALIGN_FILL);
    gridData.horizontalSpan = nColumns;
    fieldComposite.setLayoutData( gridData );

    SWTUtil.createMessageLabel (fieldComposite, UITexts.scionServer_preferences_label, 2, SWT.DEFAULT);

    serverBuiltInFieldC=new Composite(fieldComposite,SWT.NONE);
    gdata = new GridData( SWT.FILL, SWT.CENTER, true, false );
    serverBuiltInFieldC.setLayoutData( gdata );
		serverBuiltInField = new BooleanFieldEditor( IPreferenceConstants.SCION_SERVER_BUILTIN,
		                                             UITexts.scionServerBuiltIn_label,
		                                             serverBuiltInFieldC);
		serverBuiltInField.setPropertyChangeListener( new IPropertyChangeListener() {
      public void propertyChange( final PropertyChangeEvent event ) {
        updateButtonState();
        setValid( isValid() );
      }
    } );
		serverBuiltInField.setPage( this );
		serverBuiltInField.setPreferenceStore( prefStore );
		serverBuiltInField.load();

		forceRebuildC=new Composite(fieldComposite,SWT.NONE);
    gdata = new GridData( SWT.RIGHT, SWT.CENTER, true, false );
    forceRebuildC.setLayoutData( gdata );
    forceRebuild = new ButtonFieldEditor(
        UITexts.forceRebuildButton_text,
        UITexts.forceRebuildButton_label,
        new SelectionAdapter() {
          @Override
          public void widgetSelected(final SelectionEvent e) {
            rebuildBuiltin = true;
          }
        },
        forceRebuildC );
    forceRebuild.setPage( this );
    forceRebuild.setPreferenceStore( prefStore );
    //forceRebuild.fillIntoGrid( fieldComposite,2 );
    forceRebuild.load();

    serverExecutableFieldC=new Composite(fieldComposite,SWT.NONE);
    GridData gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL | GridData.VERTICAL_ALIGN_CENTER);
    gd.horizontalSpan=2;
    serverExecutableFieldC.setLayoutData( gd );
		serverExecutableField = new ExecutableFileFieldEditor(IPreferenceConstants.SCION_SERVER_EXECUTABLE,
				NLS.bind(UITexts.scionServerExecutable_label, getServerExecutableName()),
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

		autodetectC=new Composite(fieldComposite,SWT.NONE);
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

		serverFlavorFieldC = new Composite(fieldComposite, SWT.NONE);
    gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL | GridData.VERTICAL_ALIGN_CENTER);
    gd.horizontalSpan=2;
    serverFlavorFieldC.setLayoutData( gd);
		serverFlavorField = new RadioGroupFieldEditor( IPreferenceConstants.SCION_SERVER_FLAVOR,
		    UITexts.scionServerFlavor_title, 1,
		    new String[][] {
		      { UITexts.scionServerFlavor_stdstream_label, ScionManager.STDSTREAM_SCION_FLAVOR },
		      { UITexts.scionServerFlavor_network_label, ScionManager.NETWORK_SCION_FLAVOR }
		    },
		    serverFlavorFieldC, true );
		serverFlavorField.setPage(this);
		serverFlavorField.setPreferenceStore( prefStore );
		serverFlavorField.load();

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

	private void updateButtonState() {
    boolean b = serverBuiltInField.getBooleanValue();
    forceRebuild.setEnabled( b, forceRebuildC );
    autodetect.setEnabled( !b, autodetectC );
    serverExecutableField.setEnabled( !b, serverExecutableFieldC );
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

	public static String getServerExecutableName() {
		return FileUtil.makeExecutableName("scion-server"); //$NON-NLS-1$
	}

	public static void initializeDefaults(final IPreferenceStore store) {
	  // Set reasonable defaults.
	  store.setDefault( SCION_SERVER_BUILTIN, true );
	  store.setDefault( SCION_SERVER_EXECUTABLE, new String() );
	}

  @Override
  public boolean performOk() {
    cabalBlock.updateCabalImplementations();
    serverBuiltInField.store();
    serverExecutableField.store();
    autodetect.store();
    serverFlavorField.store();

    IDialogSettings settings = HaskellUIPlugin.getDefault().getDialogSettings();
    cabalBlock.saveColumnSettings( settings, PAGE_ID );

    // Yuck. You'd think there'd be a way to do this via listening for preference
    // changes, but nooooooh.
    HaskellUIPlugin.getDefault().getScionManager().handlePreferenceChanges(rebuildBuiltin);

    return super.performOk();
  }

  @Override
  public boolean performCancel() {
    return super.performCancel();
  }

  @Override
  public boolean isValid() {
    boolean retval = cabalBlock.validate( this );
    if (retval) {
      if (!serverBuiltInField.getBooleanValue()) {
        if (serverExecutableField.getStringValue().length() == 0) {
          retval = false;
          setErrorMessage( UITexts.cabalImplsBlock_needScionExecutablePath );
        }
      }
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
