package net.sf.eclipsefp.haskell.ui.internal.preferences.editor;

import net.sf.eclipsefp.common.ui.preferences.overlay.OverlayPreferenceStore;
import net.sf.eclipsefp.common.ui.util.DialogUtil;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.List;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.PreferencesUtil;

/**
 * <p>Default Editor page: appearance</p>
  *
  * @author JP Moresmau
 */
public class AppearancePP extends AbstractEditorPP {
  private final ColorListEntry[] colorListModel = new ColorListEntry[] {
        new ColorListEntry( UITexts.preferences_editor_appearance_matching_brackets_color,
                          EDITOR_MATCHING_BRACKETS_COLOR )};

    private List colorList;
    private ColorSelector colorSelector;


  @Override
  public void init( final IWorkbench workbench ) {
    super.init( workbench );
  }

  @Override
  protected void addPreferences( final OverlayPreferenceStore store ) {
    store.addBooleanKey( EDITOR_MATCHING_BRACKETS );
    store.addStringKey( EDITOR_MATCHING_BRACKETS_COLOR );
    store.addIntKey( EDITOR_TAB_WIDTH );
    store.addIntKey( EDITOR_CABAL_TAB_WIDTH );
    store.addStringKey( CA_AUTOACTIVATION_TRIGGERS );
    store.addBooleanKey( IMPORT_CLEAN_FORMAT );
  //  store.addStringKey( CA_PROPOSALS_SCOPE );
  }


  // interface methods of Tab
  ///////////////////////////
  @Override
  protected Control createContents( final Composite parent ) {

    Composite control = new Composite( parent, SWT.NONE );
    GridLayout layout = new GridLayout();
    layout.numColumns = 2;
    control.setLayout( layout );

    String text= UITexts.preferences_editor_description;
    Link link= new Link(control, SWT.NONE);
    link.setText(text);
    link.addSelectionListener(new SelectionAdapter() {
      @Override
      public void widgetSelected(final SelectionEvent e) {
        PreferencesUtil.createPreferenceDialogOn(parent.getShell(), "org.eclipse.ui.preferencePages.GeneralTextEditor", null, null); //$NON-NLS-1$
      }
    });
    link.setToolTipText(UITexts.preferences_editor_description_hover);
    GridData gd=new GridData(SWT.FILL, SWT.BEGINNING, true, false);
    gd.horizontalSpan=2;
    link.setLayoutData( gd );


    tab.addIntegerField( control, UITexts.preferences_editor_typing_tab_width, IEditorPreferenceNames.EDITOR_TAB_WIDTH, 3, 0 );
    tab.addIntegerField( control, UITexts.preferences_editor_typing_cabal_tab_width, IEditorPreferenceNames.EDITOR_CABAL_TAB_WIDTH, 3, 0 );

    addFields( control );
    createSpacer( control );
    createAppearanceColorLabel( control );
    Composite editorComposite = createEditorComposite( control );
    createColorList( control, editorComposite );
    Composite stylesComposite = createStylesComposite( editorComposite );
    tab.createLabel( stylesComposite, UITexts.preferences_editor_color);
    createColorSelector( stylesComposite );




    initialize();

    return control;
  }


  // UI creation methods
  //////////////////////

  private void createColorSelector( final Composite parent ) {
    colorSelector = new ColorSelector( parent );
    Button foregroundColorButton = colorSelector.getButton();
    GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    gridData.horizontalAlignment = GridData.BEGINNING;
    foregroundColorButton.setLayoutData( gridData );
    foregroundColorButton.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        int i = colorList.getSelectionIndex();
        String key = colorListModel[ i ].getColorKey();
        RGB colorValue = colorSelector.getColorValue();
        PreferenceConverter.setValue( getPreferenceStore(), key, colorValue );
      }
    } );
  }

  private Composite createStylesComposite( final Composite parent ) {
    Composite stylesComposite = new Composite( parent, SWT.NONE );
    GridLayout layout = new GridLayout();
    layout.marginHeight = 0;
    layout.marginWidth = 0;
    layout.numColumns = 2;
    stylesComposite.setLayout( layout );
    stylesComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    return stylesComposite;
  }

  private void createColorList( final Composite parent,
                                final Composite editorComposite ) {
    int style = SWT.SINGLE | SWT.V_SCROLL | SWT.BORDER;
    colorList = new List( editorComposite, style );
    GridData gridData = new GridData(   GridData.VERTICAL_ALIGN_BEGINNING
                                      | GridData.FILL_HORIZONTAL );
    gridData.heightHint = DialogUtil.convertHeightInCharsToPixels( parent, 8 );
    colorList.setLayoutData( gridData );
    colorList.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        handleColorListSelection();
      }
    } );
  }

  private Composite createEditorComposite( final Composite parent ) {
    Composite editorComposite = new Composite( parent, SWT.NONE );
    GridLayout layout = new GridLayout();
    layout.numColumns = 2;
    layout.marginHeight = 0;
    layout.marginWidth = 0;
    editorComposite.setLayout( layout );
    GridData gridData = new GridData(   GridData.HORIZONTAL_ALIGN_FILL
                                      | GridData.FILL_VERTICAL );
    gridData.horizontalSpan = 2;
    editorComposite.setLayoutData( gridData );
    return editorComposite;
  }

  private void createAppearanceColorLabel( final Composite parent ) {
    Label label = new Label( parent, SWT.LEFT );
    label.setText( UITexts.preferences_editor_appearance_color_options );
    GridData gridData = new GridData( GridData.HORIZONTAL_ALIGN_FILL );
    gridData.horizontalSpan = 2;
    label.setLayoutData( gridData );
  }

  private void createSpacer( final Composite parent ) {
    Label label = new Label( parent, SWT.LEFT );
    GridData gridData = new GridData( GridData.HORIZONTAL_ALIGN_FILL );
    gridData.horizontalSpan = 2;
    int height = DialogUtil.convertHeightInCharsToPixels( parent, 1 );
    gridData.heightHint = height / 2;
    label.setLayoutData( gridData );
  }

  private void addFields( final Composite parent ) {
    Label labelControl = new Label( parent, SWT.NONE );
    labelControl.setText( UITexts.preferences_editor_contentass );
    GridData gd = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
    gd.horizontalSpan=2;
    labelControl.setLayoutData( gd );

    String atText = UITexts.preferences_editor_contentass_autoactivation_triggers;
    String atKey = CA_AUTOACTIVATION_TRIGGERS;
    tab.addTextField( parent, atText, atKey, 4, 10 );

//    Label labelScope=new Label( parent, SWT.NONE );
//    labelScope.setText( UITexts.preferences_editor_contentass_scope );
//    gd = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
//    gd.horizontalIndent=10;
//    labelScope.setLayoutData( gd );
//
//    ComboViewer cv=new ComboViewer( parent,SWT.READ_ONLY );
//    cv.setContentProvider( new ArrayContentProvider() );
//    cv.setLabelProvider( new LabelProvider(){
//      @Override
//      public String getText(final Object element) {
//        ProposalScope ps=(ProposalScope)element;
//         switch (ps){
//            case ALL:
//              return UITexts.preferences_editor_contentass_scope_all;
//            case IMPORTED:
//              return UITexts.preferences_editor_contentass_scope_imported;
//            case PROJECT:
//              return UITexts.preferences_editor_contentass_scope_project;
//          }
//         return super.getText( element );
//      }
//    } );
//    cv.setInput( ProposalScope.values() );
//    cv.getCombo().setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING) );
//    cv.addSelectionChangedListener( new ISelectionChangedListener() {
//
//      @Override
//      public void selectionChanged( final SelectionChangedEvent arg0 ) {
//        ProposalScope ps=(ProposalScope)((IStructuredSelection)arg0.getSelection()).getFirstElement();
//        getPreferenceStore().setValue( CA_PROPOSALS_SCOPE, ps.toString() );
//      }
//    } );
//    ProposalScope ps=ProposalScope.valueOf(getPreferenceStore().getString( IEditorPreferenceNames.CA_PROPOSALS_SCOPE ) );
//    cv.setSelection( new StructuredSelection( ps ) );

    String mbKey = EDITOR_MATCHING_BRACKETS;
    createBooleanField( parent, UITexts.preferences_editor_appearance_matching_brackets, mbKey );

    createBooleanField( parent, UITexts.preferences_editor_appearance_import_clean_format, IMPORT_CLEAN_FORMAT );

  }

  public void propertyChange( final PropertyChangeEvent event ) {
    colorList.getDisplay().asyncExec( new Runnable() {
      @Override
      public void run() {
        if( ( colorList != null ) && !colorList.isDisposed() ) {
          handleColorListSelection();
        }
      }
    } );
    /*if (spaceForTabs!=null){
      spaceForTabs.setInfo( getFromStore( IEditorPreferenceNames.EDITOR_SPACES_FOR_TABS ) );
    }*/
  }

  // helping methods
  //////////////////

  private void handleColorListSelection() {
    int i = colorList.getSelectionIndex();
    String key = colorListModel[ i ].getColorKey();
    RGB rgb = PreferenceConverter.getColor( getPreferenceStore(), key );
    colorSelector.setColorValue( rgb );
  }

  private void initialize() {
    for( int i = 0; i < colorListModel.length; i++ ) {
      colorList.add( colorListModel[ i ].getLabel() );
    }
    colorList.getDisplay().asyncExec( new Runnable() {

      @Override
      public void run() {
        if( ( colorList != null ) && !colorList.isDisposed() ) {
          colorList.select( 0 );
          handleColorListSelection();
        }
      }
    } );
    tab.initializeFields();
  }
}
