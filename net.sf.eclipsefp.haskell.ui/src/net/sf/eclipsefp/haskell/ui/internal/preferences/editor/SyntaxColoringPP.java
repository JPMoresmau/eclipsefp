// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.preferences.editor;

import net.sf.eclipsefp.common.ui.util.DialogUtil;
import net.sf.eclipsefp.haskell.ui.internal.preferences.HaskellPreferencePage;

import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.swt.SWT;
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

/** <p>tab for syntax coloring preference settings.</p>
  *
  * @author Leif Frenzel
  *
  * TODO: The colours on this page take effect immediately, instead of waiting for Apply.
  * TODO: The Restore Defaults button does not work.
  */
public class SyntaxColoringPP extends HaskellPreferencePage implements IEditorPreferenceNames {

  private Button rbBackgroundDefault;
  private Button rbBackgroundCustom;
  private ColorSelector backgroundColorSelector;
  private ColorSelector colorSelector;
  private Button cbBold;
  private List colorList;

  private final ColorListEntry[] colorListModel = new ColorListEntry[] {
    new ColorListEntry( "Comments", EDITOR_COMMENT_COLOR, EDITOR_COMMENT_BOLD ),
    new ColorListEntry( "Literate Comments",
                        EDITOR_LITERATE_COMMENT_COLOR,
                        EDITOR_LITERATE_COMMENT_BOLD ),
    new ColorListEntry( "Strings", EDITOR_STRING_COLOR, EDITOR_STRING_BOLD ),
    new ColorListEntry( "Characters", EDITOR_CHAR_COLOR, EDITOR_CHAR_BOLD ),
    new ColorListEntry( "Functions",
                        EDITOR_FUNCTION_COLOR,
                        EDITOR_FUNCTION_BOLD ),
    new ColorListEntry( "Keywords", EDITOR_KEYWORD_COLOR, EDITOR_KEYWORD_BOLD ),
    new ColorListEntry( "Others", EDITOR_DEFAULT_COLOR, EDITOR_DEFAULT_BOLD ) };

  @Override
  public Control createContents( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );

    Group backgroundComposite = initializeBackgroundGroup( composite );
    initializeBackgroundColorSelector( backgroundComposite );
    initializeForeGroundLabel( composite );
    Composite editorComposite = initializeEditorComposite( composite );
    initializeColorList( composite, editorComposite );
    Composite stylesComposite = initializeStylesComposite( editorComposite );
    createLabel( stylesComposite, "C&olor:" );
    initializeColorSelector( stylesComposite );
    initializeBoldCheckBox( stylesComposite );

    createPreviewer( composite );
    initialize();

    return composite;
  }

  @Override
  public void initializeFields() {
    super.initializeFields();
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
    cbBold.setText( "&Bold" );
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
    label.setText( "Foreground:" );
    label.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
  }

  private Group initializeBackgroundGroup( final Composite parent ) {
    Group backgroundComposite = new Group( parent, SWT.SHADOW_ETCHED_IN );
    backgroundComposite.setLayout( new RowLayout() );
    backgroundComposite.setText( "Background color" );
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
    rbBackgroundDefault.setText( "System default" );
    rbBackgroundDefault.addSelectionListener( bgSelectionListener );
    rbBackgroundCustom = new Button( backgroundComposite, style );
    rbBackgroundCustom.setText( "Custom:" );
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