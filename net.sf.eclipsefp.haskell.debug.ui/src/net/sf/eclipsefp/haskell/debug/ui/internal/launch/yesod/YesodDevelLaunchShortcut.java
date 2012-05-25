/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.internal.launch.yesod;

import net.sf.eclipsefp.haskell.core.project.YesodNature;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.IInteractiveLaunchOperationDelegate;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.ILaunchAttributes;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.YesodLaunchDelegate;
import net.sf.eclipsefp.haskell.debug.ui.internal.launch.InteractiveLaunchOperation;
import net.sf.eclipsefp.haskell.debug.ui.internal.launch.InteractiveLaunchShortcut;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;


/**
 * Launch shortcut for yesod development server
 * @author JP Moresmau
 *
 */
public class YesodDevelLaunchShortcut extends InteractiveLaunchShortcut {

  protected IInteractiveLaunchOperationDelegate delegate;


  // interface methods of InteractiveLaunchOperation
  //////////////////////////////////////////////////

  @Override
  public IInteractiveLaunchOperationDelegate getDelegate() {
    if( delegate == null ) {
      delegate = new YesodDevelLaunchOperationDelegate();
    }
    return delegate;
  }

  @Override
  protected String getConfigTypeName() {
    return YesodLaunchDelegate.class.getName();
  }

  @Override
  protected void launch( final IResource[] resources,final String mode ) {
    try {
      IProgressMonitor monitor = new NullProgressMonitor();
      final String tn=getConfigTypeName();
      InteractiveLaunchOperation ilo=new InteractiveLaunchOperation( getDelegate() ){
        /* (non-Javadoc)
         * @see net.sf.eclipsefp.haskell.debug.ui.internal.launch.InteractiveLaunchOperation#getConfigTypeName()
         */
        @Override
        protected String getConfigTypeName() {
          return tn;
        }

        /* (non-Javadoc)
         * @see net.sf.eclipsefp.haskell.debug.ui.internal.launch.InteractiveLaunchOperation#getConfigurationId(org.eclipse.core.resources.IResource[])
         */
        @Override
        protected String getConfigurationId( final IResource[] res ) {
          // just use project name
          return res[0].getProject().getName();
        }

        /* (non-Javadoc)
         * @see net.sf.eclipsefp.haskell.debug.ui.internal.launch.InteractiveLaunchOperation#setExtraArguments(org.eclipse.debug.core.ILaunchConfigurationWorkingCopy)
         */
        @Override
        protected void setExtraArguments( final ILaunchConfigurationWorkingCopy wc ) {
          // cabal dev flag is set in preferences
          boolean cabalDev=HaskellUIPlugin.getDefault().getPreferenceStore().getBoolean( IPreferenceConstants.YESOD_CABALDEV );
          if (cabalDev){
            wc.setAttribute( ILaunchAttributes.EXTRA_ARGUMENTS,"--dev"); //$NON-NLS-1$
          } else {
            super.setExtraArguments( wc );
          }
        }
      };
      if( resources.length > 0 && resources[ 0 ] != null ) {
        IProject project = resources[ 0 ].getProject();
        if( project.hasNature( YesodNature.NATURE_ID ) ) {
          ILaunchConfiguration config = ilo.getConfiguration( resources,
                                                          new IFile[0] );
          if( config != null ) {
            config.launch( mode, monitor );
          }
        }
      }
    } catch( CoreException cex ) {
      String msg = net.sf.eclipsefp.haskell.debug.ui.internal.util.UITexts.yesod_devel_error;
      HaskellUIPlugin.log( msg, cex );
    }
  }
}
