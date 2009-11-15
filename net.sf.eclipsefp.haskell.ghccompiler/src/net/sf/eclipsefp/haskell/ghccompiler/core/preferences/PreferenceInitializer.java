// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.core.preferences;

import net.sf.eclipsefp.haskell.ghccompiler.GhcCompilerPlugin;
import net.sf.eclipsefp.haskell.ghccompiler.core.IGhcParameters;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;

/** <p>initializer for the GHC compiler preferences (declared in the
  * <code>plugin.xml</code>).</p>
  *
  * @author Leif Frenzel
  */
public class PreferenceInitializer extends AbstractPreferenceInitializer
                                   implements IGhcPreferenceNames,
                                              IGhcParameters {

  // interface methods of AbstractPreferenceInitializer
  /////////////////////////////////////////////////////

  @Override
  public void initializeDefaultPreferences() {
    IEclipsePreferences prefs = new DefaultScope().getNode( GhcCompilerPlugin.getPluginId() );
    initializeDefaultValues( prefs );
  }


  // helping methods
  //////////////////

  private void initializeDefaultValues( final IEclipsePreferences prefs ) {
    prefs.put( EXTRA_OPTIONS, "" ); //$NON-NLS-1$
    prefs.putBoolean( USE_EXTRA_OPTIONS, false );
    prefs.putInt( OPTIMIZATION_LEVEL, -1 );
//    prefs.putBoolean( GHCI_USES_GHC_OPTIONS, false );
//    prefs.putBoolean( GHCI_SOURCE_FOLDERS, true );
    initializeLanguageDefaults( prefs );
    initializeOptimizationDefaults( prefs );
    initializeMoreOptimizationDefaults( prefs );
  }

  private void initializeLanguageDefaults( final IEclipsePreferences prefs ) {
    // boolean preferences use the parameter as key
    prefs.putBoolean( LANG_GLASGOW_EXTS, false );
    prefs.putBoolean( LANG_FI, false );
    prefs.putBoolean( LANG_FFI, false );
    prefs.putBoolean( LANG_WITH, false );
    prefs.putBoolean( LANG_NO_MONOMORPHISM_RESTRICTION, false );
    prefs.putBoolean( LANG_ALLOW_OVERLAPPING_INSTANCES, false );
    prefs.putBoolean( LANG_ALLOW_UNDECIDABLE_INSTANCES, false );
    prefs.putBoolean( LANG_ALLOW_INCOHERENT_INSTANCES, false );
    prefs.putBoolean( LANG_GENERICS, false );
    prefs.putBoolean( LANG_NO_IMPLICIT_PRELUDE, false );
  }

  private void initializeOptimizationDefaults( final IEclipsePreferences prefs ) {
    // boolean preferences use the parameter as key
    prefs.putBoolean( OPT_EXCESS_PRECISION, false );
    prefs.putBoolean( OPT_IGNORE_ASSERTS, false );
    prefs.putBoolean( OPT_NO_STRICTNESS, false );
    prefs.putBoolean( OPT_NO_CPR, false );
    prefs.putBoolean( OPT_UNBOX_STRICT_FIELDS, false );
  }

  private void initializeMoreOptimizationDefaults( final IEclipsePreferences prefs ) {
    // boolean preferences use the parameter as key
    prefs.putBoolean( OPT_CASE_MERGE, false );
    prefs.putBoolean( OPT_DICTS_STRICT, false );
    prefs.putBoolean( OPT_DO_ETA_REDUCTION, false );
    prefs.putBoolean( OPT_DO_LAMBDA_ETA_EXPANSION, false );
    prefs.putBoolean( OPT_FOLDR_BUILD_ON, false );
    prefs.putBoolean( OPT_IGNORE_INTERFACE_PRAGMAS, false );
    prefs.putBoolean( OPT_LET_NO_ESCAPE, false );
    prefs.putBoolean( OPT_OMIT_INTERFACE_PRAGMAS, false );
    prefs.putBoolean( OPT_NO_CSE, false );
    prefs.putBoolean( OPT_NO_PRE_INLINING, false );
    prefs.putBoolean( OPT_NUMBERS_STRICT, false );
    prefs.putBoolean( OPT_USAGESP, false );
  }
}