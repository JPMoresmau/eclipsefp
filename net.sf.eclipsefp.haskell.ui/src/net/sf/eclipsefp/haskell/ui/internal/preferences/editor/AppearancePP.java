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
import org.eclipse.swt.widgets.List;
import org.eclipse.ui.IWorkbench;

/**
 * <p>Default Editor page: appearance</p>
  *
  * @author JP Moresmau
 */
public class AppearancePP extends AbstractEditorPP {
  private final ColorListEntry[] colorListModel = new ColorListEntry[] {
      new ColorListEntry( UITexts.preferences_editor_appearance_line_number_color,
                          EDITOR_LINE_NUMBER_RULER_COLOR ),
      new ColorListEntry( UITexts.preferences_editor_appearance_matching_brackets_color,
                          EDITOR_MATCHING_BRACKETS_COLOR ),
      new ColorListEntry( UITexts.preferences_editor_appearance_current_line_color, EDITOR_CURRENT_LINE_COLOR ),
      new ColorListEntry( UITexts.preferences_editor_appearance_print_margin, EDITOR_PRINT_MARGIN_COLOR ) };

    private List colorList;
    private ColorSelector colorSelector;

  @Override
  public void init( final IWorkbench workbench ) {
    setDescription( UITexts.preferences_editor_description );
    super.init( workbench );
  }

  @Override
  protected void addPreferences( final OverlayPreferenceStore store ) {
    store.addBooleanKey( EDITOR_CURRENT_LINE );
    store.addStringKey( EDITOR_CURRENT_LINE_COLOR );
    store.addBooleanKey( EDITOR_MATCHING_BRACKETS );
    store.addStringKey( EDITOR_MATCHING_BRACKETS_COLOR );
    store.addBooleanKey( EDITOR_PRINT_MARGIN );
    store.addStringKey( EDITOR_PRINT_MARGIN_COLOR );
    store.addIntKey( EDITOR_PRINT_MARGIN_COLUMN );
    store.addBooleanKey( EDITOR_OVERVIEW_RULER );
    store.addStringKey( EDITOR_LINE_NUMBER_RULER_COLOR );
    store.addBooleanKey( EDITOR_LINE_NUMBER_RULER );

  }


  // interface methods of Tab
  ///////////////////////////
  @Override
  protected Control createContents( final Composite parent ) {

    Composite control = new Composite( parent, SWT.NONE );
    GridLayout layout = new GridLayout();
    layout.numColumns = 2;
    control.setLayout( layout );

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
    String pmKey = EDITOR_PRINT_MARGIN_COLUMN;
    tab.addIntegerField( parent, UITexts.preferences_editor_appearance_print_margin_column, pmKey, 3, 0 );
    String orKey = EDITOR_OVERVIEW_RULER;
    createBooleanField( parent, UITexts.preferences_editor_appearance_overview_ruler, orKey );
    String lnrKey = EDITOR_LINE_NUMBER_RULER;
    createBooleanField( parent, UITexts.preferences_editor_appearance_line_numbers, lnrKey );
    String mbKey = EDITOR_MATCHING_BRACKETS;
    createBooleanField( parent, UITexts.preferences_editor_appearance_matching_brackets, mbKey );
    String clKey = EDITOR_CURRENT_LINE;
    createBooleanField( parent, UITexts.preferences_editor_appearance_current_line, clKey );
    createBooleanField( parent, UITexts.preferences_editor_appearance_print_margin, EDITOR_PRINT_MARGIN );
  }

  public void propertyChange( final PropertyChangeEvent event ) {
    colorList.getDisplay().asyncExec( new Runnable() {
      public void run() {
        if( ( colorList != null ) && !colorList.isDisposed() ) {
          handleColorListSelection();
        }
      }
    } );

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
