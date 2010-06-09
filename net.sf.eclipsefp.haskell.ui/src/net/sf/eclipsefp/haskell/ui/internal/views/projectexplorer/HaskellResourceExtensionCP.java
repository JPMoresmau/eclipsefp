// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.scion.client.OutlineHandler;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.types.OutlineDef;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.views.common.ITreeElement;
import net.sf.eclipsefp.haskell.ui.internal.views.outline.OutlineCP;
import net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer.model.GHCSystemLibrary;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.navigator.ICommonContentExtensionSite;
import org.eclipse.ui.navigator.ICommonContentProvider;

/**
 * <p>
 * content provider which is declared in <code>plugin.xml</code> for the project
 * explorer view.
 * </p>
 *
 * @author Leif Frenzel
 */
public class HaskellResourceExtensionCP implements ICommonContentProvider {


  // interface methods of ITreeContentProvider
  // //////////////////////////////////////////

  public Object[] getChildren( final Object parentElement ) {
    final List<Object> result = new ArrayList<Object>();
    try {
      if( parentElement instanceof IProject
          && ( ( IProject )parentElement ).isOpen()
          && ( ( IProject )parentElement ).hasNature( HaskellNature.NATURE_ID ) ) {
        result.add( new GHCSystemLibrary( ( IProject )parentElement ) );
        // addProjectExecutable( ( IProject )parentElement, result );
      } else if( parentElement instanceof IFile ) {
        final IFile f = ( IFile )parentElement;
        // if we have a Haskell source file, we show the same content as outline
        // underneath
        if( ResourceUtil.hasHaskellExtension( f ) && ResourceUtil.isInHaskellProject( f )) {
          final ScionInstance si = HaskellUIPlugin.getDefault()
              .getScionInstanceManager( f );
          // sync access
          if (si!=null){
            synchronized( si ) {
              si.outlineUnopenedFile( f, new OutlineHandler() {

                public void outlineResult( final List<OutlineDef> outlineDefs ) {
                  OutlineCP cp = new OutlineCP();
                  cp.inputChanged( null, null, outlineDefs );
                  for( OutlineDef def: outlineDefs ) {
                    if( def.getParentID() == null ) {
                      result.add( new ProjectExplorerOutlineDef( f, def, cp ) );
                    }
                  }
                  synchronized( si ) {
                    si.notifyAll();
                  }
                }
              } );
              try {
                si.wait( 10000 ); // 10 seconds max
              } catch( InterruptedException ie ) {
                // noop
              }
            }
          }
        }
      } else if( parentElement instanceof ITreeElement ) {
        ITreeElement treeElement = ( ITreeElement )parentElement;
        result.addAll( treeElement.getChildren() );
        // outline results are wrapped in a structure keeping the tree and the file
      } else if( parentElement instanceof ProjectExplorerOutlineDef ) {
        ProjectExplorerOutlineDef outline = ( ProjectExplorerOutlineDef )parentElement;
        result.addAll( outline.getChildren() );
      }
    } catch( final CoreException cex ) {
      HaskellUIPlugin.log( cex );
    }
    return result.toArray();
  }

  public Object getParent( final Object element ) {
    Object result = null;
    if( element instanceof ITreeElement ) {
      result = ( ( ITreeElement )element ).getParent();
    }
    return result;
  }

  public boolean hasChildren( final Object element ) {
    if( element instanceof IFile ) {
      IFile f = ( IFile )element;
      if( ResourceUtil.hasHaskellExtension( f ) ) {
        return true;
      }
      return false;
    }
    Object[] children = getChildren( element );
    return children == null ? false : children.length > 0;
  }

  public Object[] getElements( final Object inputElement ) {
    return new Object[ 0 ];
  }

  public void dispose() {
    // unused
  }

  public void inputChanged( final Viewer viewer, final Object oldInput,
      final Object newInput ) {
    // unused
  }


  // interface methods of ICommonContentProvider
  // ////////////////////////////////////////////

  // TODO lf note to self: config -> navigator service -> can be used to
  // get state information from the View
  public void init( final ICommonContentExtensionSite config ) {
    IEclipsePreferences node = new InstanceScope().getNode( HaskellCorePlugin
        .getPluginId() );
    node.addPreferenceChangeListener( new IPreferenceChangeListener() {

      public void preferenceChange( final PreferenceChangeEvent event ) {
        String prop = event.getKey();
        if( ICorePreferenceNames.HS_IMPLEMENTATIONS.equals( prop )
            || ICorePreferenceNames.SELECTED_HS_IMPLEMENTATION.equals( prop ) ) {
          config.getService().update();
        }
      }
    } );
  }

  public void restoreState( final IMemento memento ) {
    // unused
  }

  public void saveState( final IMemento memento ) {
    // unused
  }


  // helping functions
  // //////////////////

 /* private void addProjectExecutable( final IProject project,
      final List<Object> list ) {
    try {
      IFile[] executables = ResourceUtil.getProjectExecutables( project );
      list.addAll( Arrays.asList( executables ) );
    } catch( CoreException ex ) {
      String msg = "Problem determining project executable for "
          + project.getName();
      HaskellUIPlugin.log( msg, ex );
    }
  }*/
}
