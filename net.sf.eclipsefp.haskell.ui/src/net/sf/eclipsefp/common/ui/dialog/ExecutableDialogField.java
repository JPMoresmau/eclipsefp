// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.dialog;

import net.sf.eclipsefp.common.ui.util.DialogUtil;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;




/** <p>a dialog field that allows the selection of an executable which
  * can be queried for information (to be displayed on this dialog field).</p>
  *
  * @author Leif Frenzel
  */
public abstract class ExecutableDialogField extends DialogField {

  private static final String EMPTY = "No executable found.";

  private String info;

  // UI components
  private Group grpInfo;
  private Text text;
  private Text txtDisplay;

  public ExecutableDialogField( final Composite parent,
                                final String labelText ) {
    super( parent );
    setLayout( new GridLayout() );
    initGroup();
    initLabel( labelText );
    initText();
    addModifyListener();
    addFileField();
    addDisplayTextField();
  }


  // interface methods of DialogField
  ///////////////////////////////////

  @Override
  public Object getInfo() {
    return info;
  }

  @Override
  public void setInfo( final Object info ) {
    this.info = ( String )info;
    int c=text.getCaretPosition();
    text.setText( this.info );
    text.setSelection( c,c );
  }


  // to be implemented by subclasses
  //////////////////////////////////

  protected abstract String createDisplayContent( String info );


  // UI creation
  //////////////

  private void initLabel( final String labelText ) {
    Label labelControl = new Label( grpInfo, SWT.NONE );
    labelControl.setText( labelText );
    GridData gd = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
    gd.horizontalIndent = 1;
    labelControl.setLayoutData( gd );
  }

  private void initGroup() {
    grpInfo = new Group( this, SWT.NONE );
    grpInfo.setText( "Executable information" );
    GridLayout layout = new GridLayout();
    layout.numColumns = 3;
    grpInfo.setLayout( layout );
    grpInfo.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
  }

  private void addModifyListener() {
    text.addModifyListener( new ModifyListener() {
      @Override
      public void modifyText( final ModifyEvent event ) {
        Text text = ( Text )event.widget;
        info = text.getText();
        txtDisplay.setText( createDisplayContent( info ) );
        notifyListeners( info );
      }
    } );
  }

  private void addFileField() {
    Button btnBrowse = new Button( grpInfo, SWT.PUSH );
    btnBrowse.setText( "Browse..." );
    GridData gd = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
    gd.widthHint = DialogUtil.convertWidthInCharsToPixels( btnBrowse, 13 );
    btnBrowse.setLayoutData( gd );
    btnBrowse.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        FileDialog dialog = new FileDialog( text.getShell() );
        dialog.setText( "Browse to the executable." );
        String selectedFile = dialog.open();
        if( selectedFile != null ) {
          text.setText( selectedFile );
        }
      }
    } );
  }

  private void addDisplayTextField() {
    txtDisplay = new Text( grpInfo, SWT.BORDER | SWT.V_SCROLL | SWT.WRAP );
    txtDisplay.setSize( 200, 200 );
    txtDisplay.setEditable( false );
    GridData gd = new GridData( GridData.FILL_HORIZONTAL );
    gd.horizontalSpan = 3;
    txtDisplay.setLayoutData( gd );
    txtDisplay.setText( EMPTY );
  }

  private void initText() {
    text = new Text( grpInfo, SWT.BORDER | SWT.SINGLE );
    text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
  }
}