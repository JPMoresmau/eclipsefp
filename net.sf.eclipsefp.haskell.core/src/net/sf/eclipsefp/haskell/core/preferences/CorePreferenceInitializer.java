// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.preferences;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;


/** <p>initializer for the core preferences (declared in the
  * <code>plugin.xml</code>).</p>
  *
  * @author Leif Frenzel
  */
public class CorePreferenceInitializer extends AbstractPreferenceInitializer
                                       implements ICorePreferenceNames {


  // interface methods of AbstractPreferenceInitializer
  /////////////////////////////////////////////////////

  @Override
  public void initializeDefaultPreferences() {
    IEclipsePreferences coreNode = new DefaultScope().getNode( HaskellCorePlugin.getPluginId() );
    coreNode.put( SELECTED_COMPILER, "ghcCompiler" ); //$NON-NLS-1$
    coreNode.put( FOLDERS_SRC, "src" ); //$NON-NLS-1$
    coreNode.put( FOLDERS_DOC, "doc" ); //$NON-NLS-1$
   // coreNode.put( FOLDERS_OUT, "out" ); //$NON-NLS-1$
   // coreNode.put( TARGET_BINARY, "bin/theResult" ); //$NON-NLS-1$
    coreNode.putBoolean( FOLDERS_IN_NEW_PROJECT, true );

    coreNode.putBoolean( DEBUG_BREAK_ON_ERROR, false );
    coreNode.putBoolean( DEBUG_BREAK_ON_EXCEPTION, false );
    coreNode.putBoolean( DEBUG_PRINT_WITH_SHOW, true );
  }
}