package net.sf.eclipsefp.haskell.ui.wizards.cabal;

import java.io.File;
import java.util.Collection;
import java.util.Iterator;
import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * <p>Common UI class for cabal operations: set the dist folder (use scion folder by default</p>
  *
  * @author JP Moresmau
 */
public class DistFolder {
  private final Text tFolder;
  private String fullPath;

  public DistFolder(final Collection<IProject> projects,final Composite composite,final String label,final String dialogTitle,final String dialogMessage){
    Label lFolder=new Label(composite,SWT.NONE);
    lFolder.setText( label );

    tFolder=new Text(composite,SWT.BORDER);
    tFolder.setText( ScionPlugin.DIST_FOLDER );
    GridData gd=new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
    tFolder.setLayoutData( gd );

    Iterator<IProject> projIter = projects.iterator();
    if (projIter.hasNext()) {
      final IProject uniqueP=projIter.next();
      final String projectLocation=uniqueP.getLocation().toOSString();
      fullPath=uniqueP.getLocation().append( ScionPlugin.DIST_FOLDER ).toOSString();
      Button bFolder=new Button(composite,SWT.PUSH);
      bFolder.setText( "..." );
      bFolder.addSelectionListener( new SelectionAdapter() {
        @Override
        public void widgetSelected( final SelectionEvent e ) {
          DirectoryDialog dd=new DirectoryDialog( composite.getShell() );
          dd.setText( dialogTitle );
          dd.setMessage( dialogMessage );
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

  }

  public String getFolder(){
    return tFolder.getText();
  }

}
