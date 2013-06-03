// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.HaskellLaunchDelegate;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.IInteractiveLaunchOperationDelegate;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.ILaunchAttributes;
import net.sf.eclipsefp.haskell.debug.ui.internal.util.UITexts;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.osgi.util.NLS;


/** <p>encapsulates the work involved in finding a launch configuration
  * (if one exists) for the selection and launching it.</p>
  *
  * @author Leif Frenzel
  */
public class InteractiveLaunchOperation extends LaunchOperation {
  static final String INTERACTIVE_CONFIG_TYPE = HaskellLaunchDelegate.class.getName();


  private static final String FIRST_SELECTED_RESOURCE
    = "FIRST_SELECTED_RESOURCE"; //$NON-NLS-1$

  // TODO we need an infrastructural change here: must have the executable
  // launching and the generic launching for interactive sessions in some
  // debug/debug.ui plugins, and let interactive environment interfacing
  // only declare the specific stuff

  private final IInteractiveLaunchOperationDelegate delegate;

  public InteractiveLaunchOperation( final IInteractiveLaunchOperationDelegate del ) {
    this.delegate = del;
  }

  @Override
  protected String getConfigTypeName() {
    return INTERACTIVE_CONFIG_TYPE;
  }

  // methods called from outside
  //////////////////////////////

  public void launch( final IResource[] resources,final String mode,
               final IProgressMonitor monitor ) throws CoreException {
    IFile[] filesToLoad = SelectionAnalyzer.getSourcesToLoad( resources );
    if( resources.length > 0 && resources[ 0 ] != null ) {
      IProject project = resources[ 0 ].getProject();
      if(ResourceUtil.hasHaskellNature (project) ) {
        ILaunchConfiguration config = getConfiguration( resources,
                                                        filesToLoad );
        if( config != null ) {
          config.launch( mode, monitor );
        }
      }
    }
  }


  // helping methods
  //////////////////

  public ILaunchConfiguration getConfiguration( final IResource[] resources,
                                                 final IFile[] files )
                                                          throws CoreException {
    List<ILaunchConfiguration> configurations = findConfig(delegate, resources,getConfigTypeName() );
    ILaunchConfiguration result = null;
    int count = configurations.size();
    if( count < 1 ) {
      // If there are no existing configs associated with the ICompilationUnit,
      // create one.
      result = createConfiguration( resources, files );
    } else if( count == 1 ) {
      // If there is exactly one config associated with the ICompilationUnit,
      // return it.
      result = configurations.get( 0 );
    } else {
      // Otherwise, if there is more than one config associated with the
      // ICompilationUnit, prompt the user to choose one.
      result = choose( configurations );
    }
    return result;
  }

  protected String getConfigurationId(final IResource[] resources){
    IContainer src=ResourceUtil.getSourceContainer( resources[0] );
    IPath path=ResourceUtil.getSourceFolderRelativeName( resources[0] );
    path=path.removeFileExtension();
    return NLS.bind( UITexts.configuration_name, new Object[]{path.toPortableString() ,resources[0].getProject().getName(),src!=null?src.getProjectRelativePath().toPortableString():""}); //$NON-NLS-1$

  }

  /**
   * set extra arguments that we know from preferences etc
   * subclasses can override
   * @param wc
   */
  protected void setExtraArguments(final ILaunchConfigurationWorkingCopy wc){
    // no extra arguments by default
    wc.setAttribute( ILaunchAttributes.EXTRA_ARGUMENTS,
        ILaunchAttributes.EMPTY );
  }

  private ILaunchConfiguration createConfiguration( final IResource[] resources,
                                                    final IFile[] files )
                                                          throws CoreException {
    ILaunchConfigurationType configType = getConfigType();
    String s=getConfigurationId(resources);
    String id = createConfigId(s);
    IProject project = resources[ 0 ].getProject();


    ILaunchConfigurationWorkingCopy wc = configType.newInstance( null, id );
    //String exePath = delegate.getExecutable();
    //wc.setAttribute( ILaunchAttributes.EXECUTABLE, exePath );
    String projectLocation = project.getLocation().toOSString();
    wc.setAttribute( ILaunchAttributes.WORKING_DIRECTORY, projectLocation );
    //wc.setAttribute( ILaunchAttributes.ARGUMENTS,
    //                 getArguments(delegate, project, files ) );
    setExtraArguments(wc);
    wc.setAttribute( ILaunchAttributes.DELEGATE, delegate.getClass().getName() );
    List<String> fileNames=new ArrayList<String>(files.length);
    for (IFile f:files){
      fileNames.add(f.getProjectRelativePath().toPortableString());
    }
    wc.setAttribute(ILaunchAttributes.FILES,fileNames);
    wc.setAttribute(ILaunchAttributes.SYNC_STREAMS,true);
    String projectName = ILaunchAttributes.PROJECT_NAME;
    wc.setAttribute( projectName, project.getName() );
    wc.setAttribute( FIRST_SELECTED_RESOURCE, resources[ 0 ].getName() );
    //wc.setAttribute( DebugPlugin.ATTR_PROCESS_FACTORY_ID,"net.sf.eclipsefp.haskell.debug.core.launch.REPLProcessFactory");

    //wc.setAttribute( ILaunchAttributes.RELOAD_COMMAND, delegate.getReloadCommand() );
    // by default, reload when saving, that's why interactive sessions are good
    wc.setAttribute( ILaunchAttributes.RELOAD,true);
    return wc.doSave();
  }


  public static List<ILaunchConfiguration> findConfig( final IInteractiveLaunchOperationDelegate delegate, final IResource[] resources,final String configType )
                                                          throws CoreException {
    List<ILaunchConfiguration> result = Collections.emptyList();
    ILaunchConfiguration[] configurations = LaunchOperation.getConfigurations(LaunchOperation.getConfigType( configType ));
    result = new ArrayList<ILaunchConfiguration>( configurations.length );
    String cls=delegate.getClass().getName();
    String exePath = delegate.getExecutable();
    String projectName = resources[ 0 ].getProject().getName();
    String firstResName = resources[ 0 ].getName();

    for( int i = 0; i < configurations.length; i++ ) {
      ILaunchConfiguration configuration = configurations[ i ];
            if( (getDelegate( configuration )==null || getDelegate( configuration ).equals( cls ))
          && HaskellDebugCore.getProjectName( configuration ).equals( projectName )
          && getFirstResName( configuration ).equals( firstResName ) ) {
        String cExePath=getExePath( configuration );
       // ensure exe path is current
        if (cExePath==null || !cExePath.equals( exePath )){
          ILaunchConfigurationWorkingCopy wc=configuration.getWorkingCopy();
          wc.setAttribute( ILaunchAttributes.EXECUTABLE, exePath);
          configuration=wc.doSave();
        }

        result.add( configuration );
      }

    }
    return result;
  }

  private static String getFirstResName( final ILaunchConfiguration config )
                                                          throws CoreException {
    String att = FIRST_SELECTED_RESOURCE;
    return config.getAttribute( att, ILaunchAttributes.EMPTY );
  }

  public static String getDelegate(final ILaunchConfiguration config) throws CoreException{
    return config.getAttribute( ILaunchAttributes.DELEGATE, (String)null );
  }

//  public static String getArguments(final IInteractiveLaunchOperationDelegate delegate,final IProject project, final IFile[] files ) {
//    //IHaskellProject hsProject = HaskellProjectManager.get( project );
//    return concatenate( delegate.createArguments( project, files ) );
//  }

  public static String concatenate( final String[] args ) {
    StringBuffer sb = new StringBuffer();
    if( args.length > 0 ) {
      sb.append( args[ 0 ] );
    }
    for( int i = 1; i < args.length; i++ ) {
      sb.append( " " ); //$NON-NLS-1$
      sb.append( args[ i ] );
    }
    return sb.toString();
  }
}