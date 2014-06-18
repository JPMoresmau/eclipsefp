package net.sf.eclipsefp.haskell.ui.wizards.cabal;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

/**
 * <p>Options for cabal sdist</p>
  *
  * @author JP Moresmau
 */
public class CabalSDistOptionsPage extends WizardPage implements PropertyChangeListener{
  private DistFolder dFolder;
  private Button bSnapshot;
  private final Collection<IProject> projects;

  /**
   * Map ; key is project root folder, value is sdist result file stub (name+ version)
   */
  private Map<String,String> fileNamesByProjectPaths;

  public CabalSDistOptionsPage(final Collection<IProject> projects) {
    super( "SDistOptions", UITexts.exportSource_options, null );
    this.projects=projects;
    getFileNames();
  }

  @Override
  public void createControl( final Composite parent ) {
    initializeDialogUnits( parent );
    Composite composite = new Composite( parent, SWT.NONE );
    GridData gd=new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL);
    composite.setLayoutData( gd );
    int cols=projects.size()==1?3:2;
    GridLayout layout=new GridLayout( cols, false );
    composite.setLayout( layout );

    dFolder=new DistFolder(projects,composite, UITexts.exportSource_options_folder,UITexts.exportSource_options_folder_choose,UITexts.exportSource_options_folder_choose );

    bSnapshot=new Button(composite,SWT.CHECK);
    bSnapshot.setText( UITexts.exportSource_options_snapshot );
    gd=new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
    gd.horizontalSpan=3;
    bSnapshot.setLayoutData( gd );
    bSnapshot.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        propertyChange( new PropertyChangeEvent( dFolder, DistFolder.PROP_PATH, null, getFolder() ) );
      }
    });
    setControl( composite );
    dFolder.addPropertyListener( this );
    Dialog.applyDialogFont( composite );
  }

  public String getFolder(){
    return dFolder.getFolder();
  }

  public boolean isSnapshot(){
    return bSnapshot!=null && bSnapshot.getSelection();
  }

  private void getFileNames(){
    fileNamesByProjectPaths=new HashMap<>();
    for (IProject prj:projects){
      IFile cf=BuildWrapperPlugin.getCabalFile( prj);
      try {
        PackageDescription pd=PackageDescriptionLoader.load( cf );
        if (pd.getStanzas().size()>0){
          String name=pd.getStanzas().get( 0 ).getName();
          String version=pd.getStanzas().get( 0 ).getProperties().get( CabalSyntax.FIELD_VERSION );
          if (version!=null){
            fileNamesByProjectPaths.put(prj.getLocation().toOSString(), name+"-"+version);
          }
        }
      } catch (CoreException ce){
        HaskellUIPlugin.log( ce );
      }

    }
  }

  /**
   * check if the file already exists to display a warning
   */
  @Override
  public void propertyChange( final PropertyChangeEvent evt ) {
    String exists=null;
    String toDisplay=(String)evt.getNewValue();
    File f=new File(toDisplay);
    boolean abs=f.isAbsolute();

    String d="";
    /**
     * add snapshot is necessary
     */
    if (isSnapshot()){
      SimpleDateFormat sdf=new SimpleDateFormat("yyyyMMdd");
      d="."+sdf.format( new Date());
    }

    for (Map.Entry<String,String> e:fileNamesByProjectPaths.entrySet()){
      String shortName=e.getValue()+d+".tar.gz";
      File full=abs?new File(toDisplay,shortName):new File(new File(e.getKey(),toDisplay),shortName);
      if (full.exists()){
        exists=full.getAbsolutePath();
        break;
      }
    }
    if (exists!=null){
      String msg=NLS.bind( UITexts.exportSource_overwrite_warning,exists);
      setMessage( msg, WARNING );
    } else {
      setMessage( null );
    }
  }
}
