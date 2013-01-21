// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.compiler;

import java.io.BufferedReader;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.util.CoreTexts;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import net.sf.eclipsefp.haskell.core.util.GHCSyntax;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.ProcessRunner;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.osgi.util.NLS;
import org.osgi.service.prefs.BackingStoreException;

/**
 * <p>
 * manages the Haskell compilers.
 * </p>
 *
 * @author Leif Frenzel
 */
public class CompilerManager {



	/** Current Haskell implementation */
	private static IHsImplementation currentHsImplementation;

	static {
		initHsImplementation();
    if( currentHsImplementation == null ) {
      String msg =   CoreTexts.hsImplementation_none;
      HaskellCorePlugin.log( msg, IStatus.WARNING );
    }
		listenForImplPref();
	}


	/**
	 * get the path name to the compiler executable
	 * @return
	 */
	 public static String getCompilerExecutable() {
	    IPath result = null;

	    IHsImplementation impl = getCurrentHsImplementation();
	    if( impl != null && impl.getBinDir() != null ) {
	      result = new Path( impl.getBinDir() );
	      result = result.append( GHCSyntax.GHC );
	    }
	    return result == null ? GHCSyntax.GHC : result.toOSString();
	  }

	 private static List<String> extensions;

	 /**
	  * get the extensions as given by GHC
	  * @return the list of extensions known to the compiler
	  */
	  public static List<String> getExtensions(){
	    if (extensions==null){

	      File f=new File(getCompilerExecutable());
	      if (f.exists()){
	          StringWriter sw=new StringWriter();
	          try {
	            new ProcessRunner().executeBlocking( f.getParentFile(), sw, null, f.getAbsolutePath(),GHCSyntax.EXTENSIONS );
	            String s=sw.toString();
	            BufferedReader br=new BufferedReader( new StringReader( s ) );
	            extensions=new ArrayList<String>();
	            String line=br.readLine();
	            while (line!=null){
	              extensions.add(line.trim());
	              line=br.readLine();
	            }
	            Collections.sort( extensions, String.CASE_INSENSITIVE_ORDER );
	          } catch (IOException ioe){
	            HaskellCorePlugin.log( ioe );
	          }
	      }
	    }
	    return extensions;
	  }


  public static IHsImplementation getCurrentHsImplementation() {
	  return currentHsImplementation;
	}


  private static void initHsImplementation() {
    IPreferencesService prefSvc = Platform.getPreferencesService();
    String currentImplName = prefSvc.getString( HaskellCorePlugin.getPluginId(), ICorePreferenceNames.SELECTED_HS_IMPLEMENTATION, null, null );
    if( currentImplName != null ) {
      Map<String, IHsImplementation> impls = loadImpls();
      if (impls.size()>0){
        currentHsImplementation = impls.get( currentImplName );
        if (currentHsImplementation!=null){
          return;
        }
      }
    }
    List<IHsImplementation> impls=autodetectGHCImpls();
    if (impls.size()>0){
      IHsImplementation def=impls.get( 0 );
      if( currentImplName != null ) {
        for (IHsImplementation imp:impls){
          if (imp.getName().equals( currentImplName )){
            def=imp;
            break;
          }
        }
      }
      setHsImplementations(impls,def);
      initHsImplementation() ;
    }

  }

  public static void setHsImplementations(final List<IHsImplementation> impls,final IHsImplementation def){
    IEclipsePreferences node = HaskellCorePlugin.instanceScopedPreferences();
    node.put( ICorePreferenceNames.HS_IMPLEMENTATIONS, HsImplementationPersister.toXML( impls ) );
    node.put( ICorePreferenceNames.SELECTED_HS_IMPLEMENTATION, def!=null?def.getName() :""); //$NON-NLS-1$
    try {
      node.flush();
    } catch( BackingStoreException ex ) {
      HaskellCorePlugin.log( ex );
    }
  }

  private static Map<String, IHsImplementation> loadImpls() {
    List<IHsImplementation> impls = new ArrayList<IHsImplementation>();
    String xml = Platform.getPreferencesService().getString( HaskellCorePlugin.getPluginId(), ICorePreferenceNames.HS_IMPLEMENTATIONS, null, null );
    HsImplementationPersister.fromXML( xml, impls );
    Map<String, IHsImplementation> result = new HashMap<String, IHsImplementation>();
    for( IHsImplementation impl: impls ) {
      if (isValid( impl )){
        result.put( impl.getName(), impl );
      }
    }
    return result;
  }

  private static void listenForImplPref() {
    HaskellCorePlugin.instanceScopedPreferences().addPreferenceChangeListener( new IPreferenceChangeListener() {
      @Override
      public void preferenceChange( final PreferenceChangeEvent event ) {
        String key = event.getKey();
        if(    ICorePreferenceNames.HS_IMPLEMENTATIONS.equals( key )
            || ICorePreferenceNames.SELECTED_HS_IMPLEMENTATION.equals( key ) ) {
          initHsImplementation();
        }
      }
    });
  }

  private static boolean isValid(final IHsImplementation impl){
    IStatus[] statuss=impl.validate();
    boolean err=false;
    for( int i = 0; i < statuss.length; i++ ) {
      IStatus curr = statuss[ i ];
      if( curr.matches( IStatus.ERROR ) ) {
        err=true;
      }
    }
    return !err;
  }

  public static List<IHsImplementation> autodetectGHCImpls(){
    ArrayList<File> candidateLocs = FileUtil.getCandidateLocations();
    List<IHsImplementation> impls=new ArrayList<IHsImplementation>();
    for (File loc : candidateLocs) {
      File[] files = loc.listFiles( new FilenameFilter() {
        @Override
        public boolean accept( final File dir, final String name ) {
          return name.equals( GHCSyntax.GHC );
        }
      });
      if (files != null && files.length > 0) {
        for (File file : files) {
          HsImplementation impl=new HsImplementation();
          impl.setType( HsImplementationType.GHC );
          impl.setBinDir( file.getParent() );
          impl.validate();
          String vs=impl.getVersion();
          String nameStub=NLS.bind( CoreTexts.hsImplementation_name_default,impl.getType().toString(),vs);
          int index=1;
          String nameFull=nameStub;
          while( isDuplicateName(impls,nameFull,impl ) ) {
            nameFull=NLS.bind(CoreTexts.hsImplementation_name_index,nameStub,String.valueOf(index));
            index++;
          }
          impl.setName( nameFull );
          if (isValid( impl )){
            impls.add( impl );
          }
        }
      }

    }
    return impls;
  }

  public static boolean isDuplicateName(final List<IHsImplementation> impls, final String name, final HsImplementation impl ) {
    boolean result = false;
    if( name != null && name.trim().length() > 0 ) {
      for( IHsImplementation inst: impls ) {
        result |= inst!=impl && name.equals( inst.getName() );
      }
    }
    return result;
  }
}