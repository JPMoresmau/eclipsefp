/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.refactoring.wizards;

import net.sf.eclipsefp.haskell.ui.internal.refactoring.RenameDelegate;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.ltk.ui.refactoring.UserInputWizardPage;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;


/**
 * User page for rename: type in the new name and the scope of replacement (project or workspace)
 * @author JP Moresmau
 *
 */
public class RenamePage1 extends UserInputWizardPage {
  private final RenameDelegate delegate;

  /**
   * @param name
   */
  public RenamePage1( final RenameDelegate delegate ) {
    super( UITexts.renameProcessor_name );
    this.delegate=delegate;
  }


  /* (non-Javadoc)
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite arg0 ) {
    Composite c=new Composite( arg0,SWT.NONE );
    c.setLayout( new GridLayout(3,false) );

    Label l=new Label(c,SWT.NONE);
    l.setText( UITexts.renameProcessor_newname );

    final Text t=new Text(c,SWT.BORDER);
    GridData gd=new GridData(GridData.FILL_HORIZONTAL);
    gd.horizontalSpan=2;
    t.setLayoutData( gd );
    boolean valid=false;
    // we found something to rename
    if (this.delegate.getNewName()!=null){
      t.setText( this.delegate.getNewName() );
      valid=true;
    } else {
      setErrorMessage( UITexts.renameProcessor_empty );
    }
    t.setFocus();
    t.selectAll();
    l=new Label(c,SWT.NONE);
    gd=new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
    gd.horizontalSpan=2;
    l.setLayoutData( gd );
    l.setText( UITexts.renameProcessor_scope );

    final Button bProject=new Button( c, SWT.RADIO );
    gd=new GridData(GridData.FILL_HORIZONTAL);
    bProject.setLayoutData( gd );
    bProject.setText(NLS.bind( UITexts.renameProcessor_scope_project,delegate.getInfo().getSourceFile().getProject().getName()));
    bProject.setSelection( true );

    l=new Label(c,SWT.NONE);
    gd=new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
    gd.horizontalSpan=2;
    l.setLayoutData( gd );

    final Button bWorkspace=new Button( c, SWT.RADIO );
    gd=new GridData(GridData.FILL_HORIZONTAL);
    bWorkspace.setLayoutData( gd );
    bWorkspace.setText(UITexts.renameProcessor_scope_workspace);

    if (valid){
      t.addModifyListener( new ModifyListener() {

        @Override
        public void modifyText( final ModifyEvent paramModifyEvent ) {
          String s=t.getText();
         delegate.setNewName( s );
         if (s.length()>0){
           setErrorMessage( null );
         } else {
           setErrorMessage( UITexts.renameProcessor_newname_empty );
         }
        }
      } );
    } else {
      t.setEditable( false );
    }
    SelectionListener sl=new SelectionAdapter() {
      /* (non-Javadoc)
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e ) {
       if (bProject.getSelection()){
         delegate.setProject( delegate.getInfo().getSourceFile().getProject() );
       } else {
         delegate.setProject(null);
       }
      }
    };
    bProject.addSelectionListener( sl );
    bWorkspace.addSelectionListener( sl );
    c.setTabList( new Control[]{t,bProject,bWorkspace} );
    setControl( c );
  }

}
