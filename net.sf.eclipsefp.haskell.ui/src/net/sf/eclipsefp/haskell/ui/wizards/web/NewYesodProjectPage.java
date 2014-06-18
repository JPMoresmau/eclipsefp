package net.sf.eclipsefp.haskell.ui.wizards.web;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;

/**
*
* @author Alejandro Serrano
* @author JP Moresmau
*/
public class NewYesodProjectPage extends WizardNewProjectCreationPage {
  private static Map<String,String> dbs=new HashMap<>();
  static {
    dbs.put( UITexts.newYesodProjectWizard_db_sqlite , "s");
    dbs.put( UITexts.newYesodProjectWizard_db_postgresql ,"p");
    dbs.put( UITexts.newYesodProjectWizard_db_postgresqlfay, "pf");
    dbs.put( UITexts.newYesodProjectWizard_db_mysql,"mysql");
    dbs.put( UITexts.newYesodProjectWizard_db_mongodb , "mongo");
    dbs.put( UITexts.newYesodProjectWizard_db_simple , "simple");
  }


  //Text authorName;
  //Text foundation;
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
//    Label authorNameL = new Label( inner, SWT.NULL );
//    authorNameL.setText( "Author" );
//    authorName = new Text( inner, SWT.BORDER );
//    authorName.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
//    String userName=PlatformUtil.getCurrentUser();
//    if (userName==null){
//      userName="user";
//    }
//    authorName.setText( userName );
//    authorName.addModifyListener( new ModifyListener() {
//
//      @Override
//      public void modifyText( final ModifyEvent e ) {
//        verify();
//      }
//    } );
    // Foundation datatype
  //    Label foundationL = new Label( inner, SWT.NULL );
  //    foundationL.setText( "Foundation datatype" );
  //    foundation = new Text( inner, SWT.BORDER );
  //    foundation.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
  //    foundation.setText( "Foundation" );
  //    foundation.addModifyListener( new ModifyListener() {
  //
  //      @Override
  //      public void modifyText( final ModifyEvent e ) {
  //        verify();
  //      }
  //    } );
    // Type of database
    Label comboL = new Label( inner, SWT.NULL );
    comboL.setText( "Database" );
    dbType = new Combo( inner, SWT.BORDER | SWT.READ_ONLY );
    String[] items=dbs.keySet().toArray( new String[dbs.size()] );
    Arrays.sort( items,String.CASE_INSENSITIVE_ORDER );
    dbType.setItems(items);
    dbType.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    dbType.setText(UITexts.newYesodProjectWizard_db_sqlite);
  }

//  boolean verify() {
//    this.setErrorMessage( null );
//    if( authorName.getText().length() == 0 ) {
//      this.setErrorMessage( "Author name cannot be empty" );
//      return false;
//    } else if( foundation.getText().length() == 0 ) {
//      this.setErrorMessage( "Foundation datatype cannot be empty" );
//      return false;
//    } else if( !Character.isUpperCase( foundation.getText().charAt( 0 ) ) ) {
//      this.setErrorMessage( "Foundation datatype must start with uppercase letter" );
//      return false;
//    }
//    return true;
//  }
//
//  @Override
//  public boolean isPageComplete() {
//    if( !super.isPageComplete() ) {
//      return false;
//    }
//
//    return verify();
//  }
//
//  public String getAuthor() {
//    return authorName.getText();
//  }
//
//  public String getFoundation() {
//    return foundation.getText();
//  }

  public String getDatabase() {
    return dbs.get( dbType.getText());
  }
}
