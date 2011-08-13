package net.sf.eclipsefp.haskell.ui.web.wizards;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;


public class NewYesodProjectPage extends WizardNewProjectCreationPage {

  Text authorName;
  Text foundation;
  Combo dbType;

  public NewYesodProjectPage( final String pageName ) {
    super( pageName );
  }

  @Override
  public void createControl( final Composite parent ) {
    Composite all = new Composite( parent, SWT.NULL );
    GridLayout gl = new GridLayout( 1, false );
    all.setLayout( gl );
    super.createControl( all );
    Composite inner = new Composite( all, SWT.NULL );
    GridData innerD = new GridData( GridData.FILL_HORIZONTAL );
    innerD.grabExcessVerticalSpace = false;
    innerD.verticalAlignment = SWT.BEGINNING;
    inner.setLayoutData( innerD );
    GridLayout innerGl = new GridLayout( 2, false );
    inner.setLayout( innerGl );
    // Author name
    Label authorNameL = new Label( inner, SWT.NULL );
    authorNameL.setText( "Author" );
    authorName = new Text( inner, SWT.BORDER );
    authorName.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    authorName.setText( "user" );
    authorName.addModifyListener( new ModifyListener() {

      public void modifyText( final ModifyEvent e ) {
        verify();
      }
    } );
    // Foundation datatype
    Label foundationL = new Label( inner, SWT.NULL );
    foundationL.setText( "Foundation datatype" );
    foundation = new Text( inner, SWT.BORDER );
    foundation.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    foundation.setText( "Foundation" );
    foundation.addModifyListener( new ModifyListener() {

      public void modifyText( final ModifyEvent e ) {
        verify();
      }
    } );
    // Type of database
    Label comboL = new Label( inner, SWT.NULL );
    comboL.setText( "Database" );
    dbType = new Combo( inner, SWT.BORDER | SWT.READ_ONLY );
    dbType.setItems( new String[] { "SQLite", "PostgreSQL", "Minimal" } );
    dbType.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    dbType.setText( "SQLite" );
  }

  boolean verify() {
    this.setErrorMessage( null );
    if( authorName.getText().length() == 0 ) {
      this.setErrorMessage( "Author name cannot be empty" );
      return false;
    } else if( foundation.getText().length() == 0 ) {
      this.setErrorMessage( "Foundation datatype cannot be empty" );
      return false;
    } else if( !Character.isUpperCase( foundation.getText().charAt( 0 ) ) ) {
      this.setErrorMessage( "Foundation datatype must start with uppercase letter" );
      return false;
    }
    return true;
  }

  @Override
  public boolean isPageComplete() {
    if( !super.isPageComplete() ) {
      return false;
    }

    return verify();
  }

  public String getAuthor() {
    return authorName.getText();
  }

  public String getFoundation() {
    return foundation.getText();
  }

  public char getDatabase() {
    return Character.toLowerCase( dbType.getText().charAt( 0 ) );
  }
}
