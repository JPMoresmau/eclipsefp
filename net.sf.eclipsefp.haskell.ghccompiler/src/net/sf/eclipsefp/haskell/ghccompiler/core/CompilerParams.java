// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.core;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.ghccompiler.GhcCompilerPlugin;
import net.sf.eclipsefp.haskell.ghccompiler.core.preferences.IGhcPreferenceNames;
import org.eclipse.core.runtime.Platform;

/** <p>a helper that constructs compiler parameters from the current settings
  * in the preferences.</p>
  *
  * @author Leif Frenzel
  */
public class CompilerParams implements IGhcPreferenceNames{

  public CompilerParams() {
    super();
  }

  public List<String> construct(final String version) {
    List<String> result = new ArrayList<>();
    addOptimizationLevel( result );
   /* addLanguageParams( result );
    addOptimizationParams( result );
    addMoreOptimizationParams( result );*/
    for (GhcParameter p:GhcParameter.values()){
      if (GhcParameterType.LANGUAGE.equals( p.getType() ) ||GhcParameterType.OPTIMIZATION_SPECIFIC.equals( p.getType() )){
        addBooleanParam( p, result,version );
      }
    }
    addExtraOptions( result );
    return result;
  }


  // helping methods
  //////////////////

 /* private void addLanguageParams( final List<String> result ) {
    // boolean preferences use the parameter as key
    addBooleanParam( LANG_GLASGOW_EXTS, result );
    addBooleanParam( LANG_FI, result );
    addBooleanParam( LANG_FFI, result );
    addBooleanParam( LANG_WITH, result );
    addBooleanParam( LANG_NO_MONOMORPHISM_RESTRICTION, result );
    addBooleanParam( LANG_ALLOW_OVERLAPPING_INSTANCES, result );
    addBooleanParam( LANG_ALLOW_UNDECIDABLE_INSTANCES, result );
    addBooleanParam( LANG_ALLOW_INCOHERENT_INSTANCES, result );
    addBooleanParam( LANG_GENERICS, result );
    addBooleanParam( LANG_NO_IMPLICIT_PRELUDE, result );
  }

  private void addOptimizationParams( final List<String> result ) {
    // boolean preferences use the parameter as key
    addBooleanParam( OPT_EXCESS_PRECISION, result );
    addBooleanParam( OPT_IGNORE_ASSERTS, result );
    addBooleanParam( OPT_NO_STRICTNESS, result );
    addBooleanParam( OPT_NO_CPR, result );
    addBooleanParam( OPT_UNBOX_STRICT_FIELDS, result );
  }

  private void addMoreOptimizationParams( final List<String> result ) {
    // boolean preferences use the parameter as key
    addBooleanParam( OPT_CASE_MERGE, result );
    addBooleanParam( OPT_DICTS_STRICT, result );
    addBooleanParam( OPT_DO_ETA_REDUCTION, result );
    addBooleanParam( OPT_DO_LAMBDA_ETA_EXPANSION, result );
    addBooleanParam( OPT_FOLDR_BUILD_ON, result );
    addBooleanParam( OPT_IGNORE_INTERFACE_PRAGMAS, result );
    addBooleanParam( OPT_LET_NO_ESCAPE, result );
    addBooleanParam( OPT_OMIT_INTERFACE_PRAGMAS, result );
    addBooleanParam( OPT_NO_CSE, result );
    addBooleanParam( OPT_NO_PRE_INLINING, result );
    addBooleanParam( OPT_NUMBERS_STRICT, result );
    addBooleanParam( OPT_USAGESP, result );
  }*/

  private void addExtraOptions( final List<String> list ) {
    if( Platform.getPreferencesService().getBoolean( GhcCompilerPlugin.getPluginId(), USE_EXTRA_OPTIONS, false, null ) ) {
      String extras = Platform.getPreferencesService().getString( GhcCompilerPlugin.getPluginId(), EXTRA_OPTIONS, null, null );
      if( extras !=  null && !extras.trim().equals( "" ) ) { //$NON-NLS-1$
        list.add( extras );
      }
    }
  }

  private void addBooleanParam( final GhcParameter p, final List<String> list,final String version ) {
    boolean value = Platform.getPreferencesService().getBoolean( GhcCompilerPlugin.getPluginId(), p.getName(), false, null );
    if( value ) {
      list.add( p.getName(version) );
    }
  }

  private void addOptimizationLevel( final List<String> list ) {
    int level = Platform.getPreferencesService().getInt( GhcCompilerPlugin.getPluginId(), OPTIMIZATION_LEVEL, 0, null );
    if( level > -1 ) {
      String optLevel = "-O" + String.valueOf( level ); //$NON-NLS-1$
      list.add( optLevel );
    }
  }
}