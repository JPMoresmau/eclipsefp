package net.sf.eclipsefp.haskell.ui.wizards;

import java.io.File;
import java.util.Iterator;
import java.util.List;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * <p>Options for cabal sdist</p>
  *
  * @author JP Moresmau
 */
public class CabalSDistOptionsPage extends WizardPage {
  private Text tFolder;
  private Button bSnapshot;
  private final List<IProject> projects;
  private String fullPath;

  public CabalSDistOptionsPage(final List<IProject> projects) {
    super( "SDistOptions", UITexts.exportSource_options, null );
    this.projects=projects;
  }

  public void createControl( final Composite parent ) {
    initializeDialogUnits( parent );
    Composite composite = new Composite( parent, SWT.NONE );
    GridData gd=new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL);
    composite.setLayoutData( gd );
    int cols=projects.size()==1?3:2;
    GridLayout layout=new GridLayout( cols, false );
    composite.setLayout( layout );

    Label lFolder=new Label(composite,SWT.NONE);
    lFolder.setText( UITexts.exportSource_options_folder );

    tFolder=new Text(composite,SWT.BORDER);
    tFolder.setText( "dist" );
    gd=new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
    tFolder.setLayoutData( gd );

    Iterator<IProject> projIter = projects.iterator();
    if (projIter.hasNext()) {
      final IProject uniqueP=projIter.next();
      final String projectLocation=uniqueP.getLocation().toOSString();
      fullPath=uniqueP.getLocation().append( "dist" ).toOSString();
      Button bFolder=new Button(composite,SWT.PUSH);
      bFolder.setText( "..." );
      bFolder.addSelectionListener( new SelectionAdapter() {
        @Override
        public void widgetSelected( final SelectionEvent e ) {
          DirectoryDialog dd=new DirectoryDialog( getShell() );
          dd.setText( UITexts.exportSource_options_folder_choose );
          dd.setMessage( UITexts.exportSource_options_folder_choose );
          dd.setFilterPath( fullPath );
          fullPath=dd.open();
          String toDisplay=fullPath;

          if (fullPath.startsWith( projectLocation )){
            toDisplay=fullPath.substring( projectLocation.length()+File.separator.length() );
          }
          tFolder.setText( toDisplay );
        }
      });
    }

    bSnapshot=new Button(composite,SWT.CHECK);
    bSnapshot.setText( UITexts.exportSource_options_snapshot );
    gd=new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
    gd.horizontalSpan=3;
    bSnapshot.setLayoutData( gd );
    setControl( composite );
    Dialog.applyDialogFont( composite );
  }

  public String getFolder(){
    return tFolder.getText();
  }

  public boolean isSnapshot(){
    return bSnapshot.getSelection();
  }

}
