// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.preferences.editor;

import net.sf.eclipsefp.common.ui.util.DialogUtil;

import org.eclipse.jface.preference.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;

/** <p>Tab for the content assist preference settings.</p>
  * 
  * @author Leif Frenzel  
  */
class ContentAssistTab extends EditorTab implements IEditorPreferenceNames {

  private ColorListEntry[] colorListModel = new ColorListEntry[] {
    new ColorListEntry( "Completion proposal background",
                        CA_PROPOSALS_BACKGROUND ),
    new ColorListEntry( "Completion proposal foreground",
                        CA_PROPOSALS_FOREGROUND ) };

  private List colorList;
  private ColorSelector colorSelector;
  private Control txtAutoActDelay;
  private Control txtActTriggers;

  ContentAssistTab( final IPreferenceStore store ) {
    super( store );
  }


  // interface methods of Tab
  ///////////////////////////

  public Control createControl( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    GridLayout layout = new GridLayout();
    layout.numColumns = 2;
    composite.setLayout( layout );

    createFields( composite );
    createOptionsLabel( composite );
    Composite editorComposite = createEditorComposite( composite );
    createColorList( composite, editorComposite );
    Composite stylesComposite = createStylesComposite( editorComposite );
    createLabel( stylesComposite, "C&olor:" );
    createColorSelector( stylesComposite );
    initialize();
    return composite;
  }

  
  // UI creation methods
  //////////////////////
  
  private void createColorList( final Composite composite, 
                                final Composite editorComposite ) {
    int style = SWT.SINGLE | SWT.V_SCROLL | SWT.BORDER;
    colorList = new List( editorComposite, style );
    GridData gridData = new GridData(   GridData.VERTICAL_ALIGN_BEGINNING
                                      | GridData.FILL_HORIZONTAL );
    gridData.heightHint = DialogUtil.convertHeightInCharsToPixels( composite, 
                                                                   8 );
    colorList.setLayoutData( gridData );
    colorList.addSelectionListener( new SelectionAdapter() {
      public void widgetSelected( final SelectionEvent e ) {
        handleColorListSelection();
      }
    } );
  }

  private void createColorSelector( final Composite parent ) {
    colorSelector = new ColorSelector( parent );
    Button colorButton = colorSelector.getButton();
    GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    gridData.horizontalAlignment = GridData.BEGINNING;
    colorButton.setLayoutData( gridData );
    colorButton.addSelectionListener( new SelectionAdapter() {
      public void widgetSelected( final SelectionEvent e ) {
        int i = colorList.getSelectionIndex();
        String key = colorListModel[ i ].getColorKey();
        RGB colorValue = colorSelector.getColorValue();
        PreferenceConverter.setValue( getPreferenceStore(), key, colorValue );
      }
    } );
  }

  private Composite createStylesComposite( final Composite parent ) {
    GridLayout layout;
    Composite stylesComposite = new Composite( parent, SWT.NONE );
    layout = new GridLayout();
    layout.marginHeight = 0;
    layout.marginWidth = 0;
    layout.numColumns = 2;
    stylesComposite.setLayout( layout );
    stylesComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    return stylesComposite;
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

  private void createOptionsLabel( final Composite parent ) {
    Label label = new Label( parent, SWT.LEFT );
    label.setText( "Content assist colo&r options:" );
    GridData gridData = new GridData( GridData.HORIZONTAL_ALIGN_FILL );
    gridData.horizontalSpan = 2;
    label.setLayoutData( gridData );
  }

  private void createFields( final Composite parent ) {
    String aiKey = CA_AUTOINSERT;
    String aiText = "Insert single &proposals automatically";
    createBooleanField( parent, aiText, aiKey );
    String aoText = "Present proposals in &alphabetical order";
    createBooleanField( parent, aoText, CA_ORDER_PROPOSALS );
    String aaText = "&Enable auto activation";
    String aaKey = CA_AUTOACTIVATION;
    // TODO change this to the generic dialogfields mechanism
    Button autoActButton = addBooleanField( parent, aaText, aaKey, 0 );
    autoActButton.addSelectionListener( new SelectionAdapter() {
      public void widgetSelected( final SelectionEvent e ) {
        updateAutoactivationControls();
      }
    } );
    
    String adText = "Auto activation dela&y:";
    String adKey = CA_AUTOACTIVATION_DELAY;
    txtAutoActDelay = addTextField( parent, adText, adKey, 4, 0 );
    
    String atText = "Auto activation &triggers:";
    String atKey = CA_AUTOACTIVATION_TRIGGERS;
    txtActTriggers = addTextField( parent, atText, atKey, 4, 0 );
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

  private void updateAutoactivationControls() {
    boolean enabled = getPreferenceStore().getBoolean( 
        IEditorPreferenceNames.CA_AUTOACTIVATION );
    txtAutoActDelay.setEnabled( enabled );
    getLabel( txtAutoActDelay ).setEnabled( enabled );
    txtActTriggers.setEnabled( enabled );
    getLabel( txtActTriggers ).setEnabled( enabled );
  }
}