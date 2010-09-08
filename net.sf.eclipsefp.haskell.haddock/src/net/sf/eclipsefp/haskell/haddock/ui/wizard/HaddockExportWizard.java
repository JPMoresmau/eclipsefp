// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.ui.wizard;

import java.io.File;
import java.util.ArrayList;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.haddock.HaddockPlugin;
import net.sf.eclipsefp.haskell.haddock.core.HaddockInfo;
import net.sf.eclipsefp.haskell.haddock.ui.wizard.op.GenerateDocs;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IExportWizard;
import org.eclipse.ui.IWorkbench;


/** <p>The wizard for running Haddock over a set of source files.</p>
  *
  * @author Leif Frenzel
  */
public class HaddockExportWizard extends Wizard implements IExportWizard {

  HaddockInfo info = new HaddockInfo();

  public HaddockExportWizard() {
    setDefaultPageImageDescriptor( HaddockPlugin.getBanner() );
    setWindowTitle( "Generate Haddock documentation" );
    setDialogSettings( HaddockPlugin.getDefault().getDialogSettings() );
  }


  // interface methods of Wizard
  //////////////////////////////

  public void init( final IWorkbench workbench,
                    final IStructuredSelection selection ) {
    if( selection != null ) {
      applySelection( selection );
    }
    DialogSettingsUtil.load( getDialogSettings(), info );
  }

  @Override
  public boolean performFinish() {
    DialogSettingsUtil.save( getDialogSettings(), info );
    // TODO
System.out.println(
  "Running Haddock on " + info.getFileList().length + " files."  );
System.out.println( new GenerateDocs( info ).run() );
    return true;
  }

  @Override
  public boolean performCancel() {
    DialogSettingsUtil.save( getDialogSettings(), info );
    return true;
  }

  @Override
  public void addPages() {
    addPage( new SelectionPage( info ) );
    addPage( new OptionsPage( info ) );
    addPage( new InterfacePage( info ) );
  }


  // helping methods
  //////////////////

  private void applySelection( final IStructuredSelection ssel ) {
    ArrayList<IProject> projects = new ArrayList<IProject>();
    Object[] elems = ssel.toArray();
    for( Object elem : elems ) {
      IResource res = ResourceUtil.findResource( elem );
      if( res != null && res instanceof IProject ) {
        projects.add( (IProject) res );
      }
    }
    info.setProjects( projects );
    if (projects.size() > 0) {
      for (IProject proj : projects) {
        String projectLoc = proj.getLocation().toOSString();
        info.setOutputDir( projectLoc + File.separator + "docs" );
      }
    } else {
      info.setOutputDir( "" );
    }
  }
}
