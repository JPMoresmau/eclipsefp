package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
import net.sf.eclipsefp.haskell.debug.core.internal.util.CoreTexts;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.debug.core.model.IProcess;

/**
 *
 * @author JP Moresmau
 *
 */
public class CommandOnChangeListener implements IResourceChangeListener {
  private final IProcess p;
  private final String[] commands;
  private final String projectName;

  public CommandOnChangeListener( final IProcess p, final String projectName, final String... command ) {
    super();
    this.p = p;
    this.commands = command;
    this.projectName=projectName;
  }

  @Override
  public void resourceChanged( final IResourceChangeEvent event ) {
    try {
      event.getDelta().accept( new IResourceDeltaVisitor() {

        @Override
        public boolean visit( final IResourceDelta delta )
            throws CoreException {
          if (delta.getResource().getProject()==null || delta.getResource().getProject().getName().equals(projectName)){
            //&& (delta.getFlags() & IResourceDelta.MARKERS)>0
            if( delta.getKind() == IResourceDelta.REMOVED || (delta.getKind() == IResourceDelta.CHANGED && (delta.getFlags() & IResourceDelta.CONTENT)>0  )) {
              if( delta.getResource() instanceof IFile
                  && FileUtil.hasHaskellExtension( delta.getResource() ) ) {
                IPath relPath = delta.getResource().getProjectRelativePath();
                boolean skip = relPath.segmentCount() > 0
                    && relPath.segment( 0 ) == BWFacade.DIST_FOLDER;
                if (!skip){
                  runCommand();
                }
                return false;
              }
            }
            return true;
          }
          return false;

        }

      });
      } catch( CoreException ex ) {
        HaskellDebugCore.log(CoreTexts.commandonchange_failed, ex );
      }
  }

  protected void runCommand() throws CoreException {
    for (String s:commands){
      HaskellLaunchDelegate.commandToProcess( p, s );
    }
  }
}
