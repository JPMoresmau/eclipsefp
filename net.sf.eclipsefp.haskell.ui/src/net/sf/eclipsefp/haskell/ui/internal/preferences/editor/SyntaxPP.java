package net.sf.eclipsefp.haskell.ui.internal.preferences.editor;

import java.text.NumberFormat;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Locale;
import net.sf.eclipsefp.common.ui.preferences.overlay.OverlayPreferenceStore;
import net.sf.eclipsefp.common.ui.util.DialogUtil;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;


public class SyntaxPP extends AbstractEditorPP {
  private Button rbBackgroundDefault;
  private Button rbBackgroundCustom;
  private ColorSelector backgroundColorSelector;
  private ColorSelector colorSelector;
  private Button cbBold;
  private List colorList;

  private Text coloringThreadT;

  public static final ColorListEntry[] colorListModel = new ColorListEntry[] {
    new ColorListEntry( UITexts.preferences_editor_syntax_comments, EDITOR_COMMENT_COLOR, EDITOR_COMMENT_BOLD ),
    new ColorListEntry( UITexts.preferences_editor_syntax_literatecomments,
                        EDITOR_LITERATE_COMMENT_COLOR,
                        EDITOR_LITERATE_COMMENT_BOLD ),
    new ColorListEntry( UITexts.preferences_editor_syntax_doc, EDITOR_DOC_COLOR, EDITOR_DOC_BOLD ),
    new ColorListEntry( UITexts.preferences_editor_syntax_pragmas, EDITOR_PRAGMA_COLOR, EDITOR_PRAGMA_BOLD ),
    new ColorListEntry( UITexts.preferences_editor_syntax_strings, EDITOR_STRING_COLOR, EDITOR_STRING_BOLD ),
    new ColorListEntry( UITexts.preferences_editor_syntax_characters, EDITOR_CHAR_COLOR, EDITOR_CHAR_BOLD ),
    new ColorListEntry( UITexts.preferences_editor_syntax_numbers,
        EDITOR_NUMBER_COLOR,
        EDITOR_NUMBER_BOLD ),
//    new ColorListEntry( ScionText.preferences_editor_syntax_functions,
//                        EDITOR_FUNCTION_COLOR,
//                        EDITOR_FUNCTION_BOLD ),
    new ColorListEntry( UITexts.preferences_editor_syntax_keywords, EDITOR_KEYWORD_COLOR, EDITOR_KEYWORD_BOLD ),
    new ColorListEntry( UITexts.preferences_editor_syntax_vars, EDITOR_VAR_COLOR, EDITOR_VAR_BOLD ),
    new ColorListEntry( UITexts.preferences_editor_syntax_varsyms, EDITOR_VARSYM_COLOR, EDITOR_VARSYM_BOLD ),
    new ColorListEntry( UITexts.preferences_editor_syntax_cons, EDITOR_CON_COLOR, EDITOR_CON_BOLD ),
    new ColorListEntry( UITexts.preferences_editor_syntax_symbols, EDITOR_SYMBOL_COLOR, EDITOR_SYMBOL_BOLD ),
    new ColorListEntry( UITexts.preferences_editor_syntax_others, EDITOR_DEFAULT_COLOR, EDITOR_DEFAULT_BOLD ),
    new ColorListEntry( UITexts.preferences_editor_syntax_cpp, EDITOR_CPP_COLOR, EDITOR_CPP_BOLD ),
    new ColorListEntry( UITexts.preferences_editor_syntax_th, EDITOR_TH_COLOR, EDITOR_TH_BOLD ) };
  {
    // order by label (see compareTo in ColorListEntry)
    Arrays.sort( colorListModel );
  }


  @Override
  protected void addPreferences( final OverlayPreferenceStore store ) {
    store.addStringKey( EDITOR_FOREGROUND_COLOR );
    store.addBooleanKey( EDITOR_FOREGROUND_DEFAULT_COLOR );
    store.addStringKey( EDITOR_BACKGROUND_COLOR );
    store.addBooleanKey( EDITOR_BACKGROUND_DEFAULT_COLOR );
    store.addStringKey( EDITOR_COMMENT_COLOR );
    store.addBooleanKey( EDITOR_COMMENT_BOLD );
    store.addStringKey( EDITOR_LITERATE_COMMENT_COLOR );
    store.addBooleanKey( EDITOR_LITERATE_COMMENT_BOLD );
    store.addStringKey( EDITOR_DOC_COLOR );
    store.addBooleanKey( EDITOR_DOC_BOLD );
    store.addStringKey( EDITOR_PRAGMA_COLOR );
    store.addBooleanKey( EDITOR_PRAGMA_BOLD );
    store.addStringKey( EDITOR_STRING_COLOR );
    store.addBooleanKey( EDITOR_STRING_BOLD );
    store.addStringKey( EDITOR_CHAR_COLOR );
    store.addBooleanKey( EDITOR_CHAR_BOLD );
    store.addStringKey( EDITOR_FUNCTION_COLOR );
    store.addBooleanKey( EDITOR_FUNCTION_BOLD );
    store.addStringKey( EDITOR_NUMBER_COLOR );
    store.addBooleanKey( EDITOR_NUMBER_BOLD );
    store.addStringKey( EDITOR_VAR_COLOR );
    store.addBooleanKey( EDITOR_VAR_BOLD );
    store.addStringKey( EDITOR_VARSYM_COLOR );
    store.addBooleanKey( EDITOR_VARSYM_BOLD );
    store.addStringKey( EDITOR_CON_COLOR );
    store.addBooleanKey( EDITOR_CON_BOLD );
    store.addStringKey( EDITOR_KEYWORD_COLOR );
    store.addBooleanKey( EDITOR_KEYWORD_BOLD );
    store.addStringKey( EDITOR_SYMBOL_COLOR );
    store.addBooleanKey( EDITOR_SYMBOL_BOLD );
    store.addStringKey( EDITOR_CPP_COLOR );
    store.addBooleanKey( EDITOR_CPP_BOLD );
    store.addStringKey( EDITOR_TH_COLOR );
    store.addBooleanKey( EDITOR_TH_BOLD );
    store.addStringKey( EDITOR_DEFAULT_COLOR );
    store.addBooleanKey( EDITOR_DEFAULT_BOLD );

    store.addIntKey( EDITOR_COLORING_THREAD_THRESHOLD );
  }

  @Override
  protected Control createContents( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );

    Group backgroundComposite = initializeBackgroundGroup( composite );
    initializeBackgroundColorSelector( backgroundComposite );
    initializeForeGroundLabel( composite );
    Composite editorComposite = initializeEditorComposite( composite );
    initializeColorList( composite, editorComposite );
    Composite stylesComposite = initializeStylesComposite( editorComposite );
    tab.createLabel( stylesComposite, UITexts.preferences_editor_color );
    initializeColorSelector( stylesComposite );
    initializeBoldCheckBox( stylesComposite );

    createPreviewer( composite );

    createColoringThreadThreshold(composite);

    initialize();

    return composite;
  }

  @Override
  public void init( final IWorkbench workbench ) {
    setDescription( UITexts.preferences_editor_syntax_title);
    super.init( workbench );
  }

  public void initializeFields() {
    tab.initializeFields();
    IPreferenceStore ps = getPreferenceStore();
    RGB rgb = PreferenceConverter.getColor( ps, EDITOR_BACKGROUND_COLOR );
    backgroundColorSelector.setColorValue( rgb );
    boolean defaultBackgroud = ps.getBoolean( EDITOR_BACKGROUND_DEFAULT_COLOR );
    rbBackgroundDefault.setSelection( defaultBackgroud );
    rbBackgroundCustom.setSelection( !defaultBackgroud );
    backgroundColorSelector.setEnabled( !defaultBackgroud );
  }


  // ui initialization methods
  ////////////////////////////
  private void initializeColorList( final Composite composite,
                                    final Composite editorComposite ) {
    colorList = new List( editorComposite,
                          SWT.SINGLE | SWT.V_SCROLL | SWT.BORDER );
    GridData gridData = new GridData( GridData.FILL_BOTH );
    gridData.heightHint = DialogUtil.convertHeightInCharsToPixels( composite,
                                                                   5 );
    colorList.setLayoutData( gridData );
    colorList.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        handleSyntaxColorListSelection();
      }
    } );
  }

  private Composite initializeStylesComposite( final Composite parent ) {
    Composite stylesComposite = new Composite( parent, SWT.NONE );
    GridLayout layout = new GridLayout();
    layout.marginHeight = 0;
    layout.marginWidth = 0;
    layout.numColumns = 2;
    stylesComposite.setLayout( layout );
    stylesComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    return stylesComposite;
  }

  private void initializeBoldCheckBox( final Composite parent ) {
    cbBold = new Button( parent, SWT.CHECK );
    cbBold.setText( UITexts.preferences_editor_syntax_bold );
    GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    gridData.horizontalAlignment = GridData.BEGINNING;
    gridData.horizontalSpan = 2;
    cbBold.setLayoutData( gridData );
    cbBold.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        int i = colorList.getSelectionIndex();
        String key = colorListModel[ i ].getBoldKey();
        getPreferenceStore().setValue( key, cbBold.getSelection() );
      }
    } );
  }

  private Composite initializeEditorComposite( final Composite parent ) {
    Composite editorComposite = new Composite( parent, SWT.NONE );
    GridLayout layout = new GridLayout();
    layout.numColumns = 2;
    layout.marginHeight = 0;
    layout.marginWidth = 0;
    editorComposite.setLayout( layout );
    GridData gridData = new GridData( GridData.FILL_BOTH );
    editorComposite.setLayoutData( gridData );
    return editorComposite;
  }

  private void initializeForeGroundLabel( final Composite parent ) {
    Label label = new Label( parent, SWT.LEFT );
    label.setText( UITexts.preferences_editor_syntax_foreground );
    label.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
  }

  private Group initializeBackgroundGroup( final Composite parent ) {
    Group backgroundComposite = new Group( parent, SWT.SHADOW_ETCHED_IN );
    backgroundComposite.setLayout( new RowLayout() );
    backgroundComposite.setText( UITexts.preferences_editor_syntax_background );
    SelectionListener bgSelectionListener = new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        boolean custom = rbBackgroundCustom.getSelection();
        backgroundColorSelector.setEnabled( custom );
        getPreferenceStore().setValue(
            IEditorPreferenceNames.EDITOR_BACKGROUND_DEFAULT_COLOR, !custom );
      }
    };
    int style = SWT.RADIO | SWT.LEFT;
    rbBackgroundDefault = new Button( backgroundComposite, style );
    rbBackgroundDefault.setText( UITexts.preferences_editor_syntax_systemdefault);
    rbBackgroundDefault.addSelectionListener( bgSelectionListener );
    rbBackgroundCustom = new Button( backgroundComposite, style );
    rbBackgroundCustom.setText( UITexts.preferences_editor_syntax_custom );
    rbBackgroundCustom.addSelectionListener( bgSelectionListener );
    return backgroundComposite;
  }

  private void initializeBackgroundColorSelector( final Group parent ) {
    backgroundColorSelector = new ColorSelector( parent );
    Button backgroundColorButton = backgroundColorSelector.getButton();
    backgroundColorButton.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        String name = IEditorPreferenceNames.EDITOR_BACKGROUND_COLOR;
        RGB colorValue = backgroundColorSelector.getColorValue();
        PreferenceConverter.setValue( getPreferenceStore(), name, colorValue );
      }
    } );
  }

  private void initializeColorSelector( final Composite parent ) {
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

  private void createPreviewer( final Composite parent ) {
    IPreferenceStore preferenceStore = getPreferenceStore();
    SyntaxPreviewer previewer = new SyntaxPreviewer( parent, preferenceStore );
    GridData gridData = new GridData( GridData.FILL_BOTH );
    gridData.widthHint = DialogUtil.convertWidthInCharsToPixels( parent, 20 );
    gridData.heightHint = DialogUtil.convertHeightInCharsToPixels( parent, 5 );
    previewer.getControl().setLayoutData( gridData );
  }

  private void createColoringThreadThreshold(final Composite parent){

    Composite cColoring=new Composite(parent,SWT.NONE);
    GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    cColoring.setLayoutData( gridData );

    cColoring.setLayout( new GridLayout(2,false) );

    Label lColoring=new Label(cColoring,SWT.NONE);
    lColoring.setText( UITexts.preferences_editor_syntax_thread_threshold );
    GridData gd=new GridData( GridData.FILL_HORIZONTAL);
    gd.horizontalSpan=2;
    lColoring.setLayoutData(gd);

    coloringThreadT=new Text(cColoring,SWT.BORDER | SWT.RIGHT);
    coloringThreadT.setLayoutData( new GridData( GridData.FILL_HORIZONTAL) );

    final NumberFormat nf=NumberFormat.getIntegerInstance( Locale.getDefault() );

    int t=getPreferenceStore().getInt( EDITOR_COLORING_THREAD_THRESHOLD );
    coloringThreadT.setText( nf.format( t ));
    coloringThreadT.addModifyListener( new ModifyListener() {

      @Override
      public void modifyText( final ModifyEvent paramModifyEvent ) {
        String s=coloringThreadT.getText();
        try {
          int t=nf.parse( s ).intValue();
          setErrorMessage( null );
          getPreferenceStore().setValue( EDITOR_COLORING_THREAD_THRESHOLD, t );
        } catch (ParseException pe){
          setErrorMessage( pe.getLocalizedMessage() );
        }

      }
    } );
    Label lNote=new Label(cColoring,SWT.NONE);
    lNote.setText( UITexts.preferences_editor_syntax_thread_threshold_note );
    lNote.setLayoutData( new GridData( GridData.FILL_HORIZONTAL) );

  }

  public void propertyChange( final PropertyChangeEvent event ) {
    colorList.getDisplay().asyncExec( new Runnable() {
      @Override
      public void run() {
        if( ( colorList != null ) && !colorList.isDisposed() ) {
          handleSyntaxColorListSelection();
        }
      }
    } );

  }



  // helping methods
  //////////////////

  private void handleSyntaxColorListSelection() {
    int index = colorList.getSelectionIndex();
    String key = colorListModel[ index ].getColorKey();
    RGB rgb = PreferenceConverter.getColor( getPreferenceStore(), key );
    colorSelector.setColorValue( rgb );
    key = colorListModel[ index ].getBoldKey();
    cbBold.setSelection( getPreferenceStore().getBoolean( key ) );
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
          handleSyntaxColorListSelection();
        }
      }
    } );
    initializeFields();
  }
}
