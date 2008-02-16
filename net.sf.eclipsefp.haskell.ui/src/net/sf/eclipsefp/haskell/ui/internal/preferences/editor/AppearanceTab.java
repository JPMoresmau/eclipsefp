// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.preferences.editor;

import net.sf.eclipsefp.common.ui.util.DialogUtil;

import org.eclipse.jface.preference.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;

/** <p>the tab for appearance preference settings.</p>
  *
  * @author Leif Frenzel  
  */
class AppearanceTab extends EditorTab implements IEditorPreferenceNames {

  private final ColorListEntry[] colorListModel = new ColorListEntry[] {
    new ColorListEntry( "Line number foreground",
                        EDITOR_LINE_NUMBER_RULER_COLOR ),
    new ColorListEntry( "Matching brackets highlight",
                        EDITOR_MATCHING_BRACKETS_COLOR ),
    new ColorListEntry( "Current line highlight", EDITOR_CURRENT_LINE_COLOR ),
    new ColorListEntry( "Print margin", EDITOR_PRINT_MARGIN_COLOR ) };
  
  private List colorList;
  private ColorSelector colorSelector;

  
  AppearanceTab( final IPreferenceStore store ) {
    super( store );
  }

  
  // interface methods of Tab
  ///////////////////////////
  
  @Override
  public Control createControl( final Composite parent ) {
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
    createLabel( stylesComposite, "C&olor:" );
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
    label.setText( "Appearance co&lor options:" );
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
    addTextField( parent, "Print margin col&umn:", pmKey, 3, 0 );
    String orKey = EDITOR_OVERVIEW_RULER;
    createBooleanField( parent, "Show overview &ruler", orKey );
    String lnrKey = EDITOR_LINE_NUMBER_RULER;
    createBooleanField( parent, "Show lin&e numbers", lnrKey );
    String mbKey = EDITOR_MATCHING_BRACKETS;
    createBooleanField( parent, "Highlight &matching brackets", mbKey );
    String clKey = EDITOR_CURRENT_LINE;
    createBooleanField( parent, "Hi&ghlight current line", clKey );
    createBooleanField( parent, "Sho&w print margin", EDITOR_PRINT_MARGIN );
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
    initializeFields();
  }
}