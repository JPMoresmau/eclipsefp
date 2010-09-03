package net.sf.eclipsefp.haskell.ui.properties;

import net.sf.eclipsefp.haskell.core.code.ModuleCreationInfo;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.wizards.ModuleCreationOperation;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
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

  @Override
  protected Control createContents( final Composite parent ) {
    mic=new ModuleInclusionComposite( parent, SWT.NONE );
    IFile f=(IFile)getElement();

    IContainer src=ResourceUtil.getSourceContainer( f );
    if (src!=null){
      info=new ModuleCreationInfo();
      info.setSourceContainer( src );
      IPath p=ResourceUtil.getSourceRelativePath( src, f );
      info.setFolders(p);
      info.setModuleName( f.getProjectRelativePath().removeFileExtension().lastSegment() );
      mic.init( src, info.getQualifiedModuleName() ,false);
    } else {
      mic.initNoSourceFolder();
    }
    Dialog.applyDialogFont( parent );
    return mic;
  }

  @Override
  public boolean performOk() {
    if (info!=null){
      info.setExposed( mic.getExposed() );
      info.setIncluded( mic.getIncluded() );
      ModuleCreationOperation mco=new ModuleCreationOperation( info );
      mco.setGeneratedFile( (IFile)getElement() );

      try {
        mco.run( null );
        return true;
      } catch( Exception ex ) {
        HaskellUIPlugin.log( UITexts.module_inclusion_error, ex );
        return false;
      }
    }
    return true;
  }


}
