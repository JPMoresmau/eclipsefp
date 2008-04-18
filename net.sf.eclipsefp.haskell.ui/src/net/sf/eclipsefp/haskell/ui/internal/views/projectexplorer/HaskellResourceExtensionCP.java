// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.views.common.ITreeElement;
import net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer.model.GHCSystemLibrary;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Preferences;
import org.eclipse.core.runtime.Preferences.IPropertyChangeListener;
import org.eclipse.core.runtime.Preferences.PropertyChangeEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.navigator.ICommonContentExtensionSite;
import org.eclipse.ui.navigator.ICommonContentProvider;

/** <p>content provider which is declared in <code>plugin.xml</code> for
  * the project explorer view.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellResourceExtensionCP implements ICommonContentProvider {


  // interface methods of ITreeContentProvider
  ////////////////////////////////////////////

  public Object[] getChildren( final Object parentElement ) {
    List<Object> result = new ArrayList<Object>();
    try {
      if(    parentElement instanceof IProject
          && ( ( IProject )parentElement ).isOpen()
          && ( ( IProject )parentElement ).hasNature( HaskellNature.NATURE_ID ) ) {
        result.add( new GHCSystemLibrary( ( IProject )parentElement ) );
        addProjectExecutable( ( IProject )parentElement, result );
      } else if( parentElement instanceof ITreeElement ) {
        ITreeElement treeElement = ( ITreeElement )parentElement;
        result.addAll( treeElement.getChildren() );
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
    Object[] children = getChildren( element );
    return children == null ? false : children.length > 0;
  }

  public Object[] getElements( final Object inputElement ) {
    return new Object[ 0 ];
  }

  public void dispose() {
    // unused
  }

  public void inputChanged( final Viewer viewer,
                            final Object oldInput,
                            final Object newInput ) {
    // unused
  }


  // interface methods of ICommonContentProvider
  //////////////////////////////////////////////

  // TODO lf note to self: config -> navigator service -> can be used to
  //                       get state information from the View
  public void init( final ICommonContentExtensionSite config ) {
    Preferences prefs = HaskellCorePlugin.getDefault().getPluginPreferences();
    prefs.addPropertyChangeListener( new IPropertyChangeListener() {
      public void propertyChange( final PropertyChangeEvent event ) {
        String prop = event.getProperty();
        if(    ICorePreferenceNames.HS_IMPLEMENTATIONS.equals( prop )
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
  ////////////////////

  private void addProjectExecutable( final IProject project,
                                     final List<Object> list ) {
    try {
      IFile[] executables = ResourceUtil.getProjectExecutables( project );
      list.addAll( Arrays.asList( executables ) );
    } catch( CoreException ex ) {
      String msg =   "Problem determining project executable for "
                   + project.getName();
      HaskellUIPlugin.log( msg, ex );
    }
  }
}
