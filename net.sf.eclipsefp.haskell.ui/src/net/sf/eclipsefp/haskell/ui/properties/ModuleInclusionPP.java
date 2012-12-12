package net.sf.eclipsefp.haskell.ui.properties;

import net.sf.eclipsefp.haskell.core.code.ModuleCreationInfo;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.wizards.ModuleCreationOperation;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.dialogs.PropertyPage;

/**
 * <p>Property page for Haskell files: decide to include or expose the module</p>
  *
  * @author JP Moresmau
 */
public class ModuleInclusionPP extends PropertyPage {
  private ModuleInclusionComposite mic;
  private ModuleCreationInfo info;
  public ModuleInclusionPP() {
    // NOOP
  }

  protected IFile getFile(){
    Object o=getElement();
    if (o instanceof IFileEditorInput){
      return ((IFileEditorInput)o).getFile();
    }
    return (IFile)o;
  }

  @Override
  protected Control createContents( final Composite parent ) {
    mic=new ModuleInclusionComposite( parent, SWT.NONE );
    IFile f=getFile();

    IContainer src=ResourceUtil.getSourceContainer( f );
    if (src!=null){
      info=new ModuleCreationInfo();
      info.setProject( f.getProject() );
      info.setSourceContainer( src );
      IPath p=ResourceUtil.getSourceRelativePath( src, f );
      info.setFolders(p);
      info.setModuleName( f.getProjectRelativePath().removeFileExtension().lastSegment() );
      mic.init(f, src, info.getQualifiedModuleName() ,false);
    } else {
      mic.initNoSourceFolder();
    }
    Dialog.applyDialogFont( parent );
    return mic;
  }

  @Override
  public boolean performOk() {
    if (info!=null){
      mic.populateInfo( info );
      ModuleCreationOperation mco=new ModuleCreationOperation( info );
      mco.setGeneratedFile( getFile() );

      try {
        mco.run( new NullProgressMonitor() );
        return true;
      } catch( Exception ex ) {
        HaskellUIPlugin.log( UITexts.module_inclusion_error, ex );
        return false;
      }
    }
    return true;
  }


}
