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
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.core.HaskellCoreException;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.util.CoreTexts;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import net.sf.eclipsefp.haskell.core.util.GHCSyntax;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.ProcessRunner;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
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
public class CompilerManager implements ICompilerManager {


  private static final String ATT_NAME = "name"; //$NON-NLS-1$

  /** The singleton instance of CompilerManager. Note that this uses the current "lazy"
   *  idiom for initializing singleton instances that avoids double synchronization and
   *  other nasty hacks for thread safety (see the "Bill Pugh" solution.) */
  private static class CompilerManagerHolder {
    private static final ICompilerManager theInstance = new CompilerManager();
  }

	private static final String DEFAULT = DefaultHaskellCompiler.class.getName();

	/**
	 * the currently selected compiler. This is the compiler that is obtained by
	 * builders when they query getCompiler(). After registering a compiler, it
	 * has to be selected with selectCompiler. By the default, the
	 * DefaultCompiler (a dummy object that does no compiling itself) is
	 * selected.
	 */
	private String selectedCompiler = DEFAULT;

	/**
	 * registered compilers.
	 *
	 * keys - compiler id value - compiler information as provided in the
	 * plugin.xml
	 */
	private final Hashtable<String, IConfigurationElement> htRegisteredCompilers;

	/**
	 * installed compilers in the compiler manager. A compiler is installed
	 * (objects are created, inits are performed) only if it is used. Then it is
	 * cached here.
	 *
	 * key - compiler id value - the compiler object (which implements
	 * IHaskellCompiler)
	 */
	private final Hashtable<String, ListenableCompilerDecorator> htInstalledCompilers;

	/** Current Haskell implementation */
	private IHsImplementation currentHsImplementation;

	/**
	 * Creates the singleton instance of CompilerManager.
	 */
	private CompilerManager() {
		htInstalledCompilers = new Hashtable<String, ListenableCompilerDecorator>();
		htRegisteredCompilers = new Hashtable<String, IConfigurationElement>();
		initDefaultCompiler();
		initHsImplementation();
    if( currentHsImplementation == null ) {
      String msg =   CoreTexts.hsImplementation_none;
      HaskellCorePlugin.log( msg, IStatus.WARNING );
    }
		listenForImplPref();
	}

  /** Get the CompilerManager instance. */
	public static final ICompilerManager getInstance() {
		return CompilerManagerHolder.theInstance;
	}

	/**
	 * get the path name to the compiler executable
	 * @return
	 */
	 public static String getCompilerExecutable() {
	    IPath result = null;

	    ICompilerManager msn = CompilerManager.getInstance();
	    IHsImplementation impl = msn.getCurrentHsImplementation();
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

	/**
	 * <p>
	 * returns the ids of all compilers registered with the compiler manager.
	 * </p>
	 */
	@Override
  public String[] getRegisteredCompilers() {
		int size = htRegisteredCompilers.size();
		String[] result = new String[size + 1];
		htRegisteredCompilers.keySet().toArray(result);
		result[size] = DEFAULT;
		return result;
	}

	/**
	 * <p>
	 * returns the currently used Haskell compiler.
	 * </p>
	 */
	@Override
  public IHaskellCompiler getCompiler() {
		return htInstalledCompilers.get(selectedCompiler);
	}

	/**
	 * <p>
	 * if the compiler specified by id is known in the compiler manager, it is
	 * selected and will be the one returned by getCompiler() from now on.
	 * </p>
	 */
	@Override
  public boolean selectCompiler(final String id) throws Exception {
		boolean result = false;
		if (htInstalledCompilers.containsKey(id)) {
			selectedCompiler = id;
			result = true;
		} else if (htRegisteredCompilers.containsKey(id)) {
			try {
				installCompiler(id);
			} catch (HaskellCoreException hcEex) {
				throw new Exception("Could not install compiler '" + id //$NON-NLS-1$
						+ "'.\n", hcEex); //$NON-NLS-1$
			}
			selectedCompiler = id;
			result = true;
		} else {
			fireHCEx("No Haskell compiler registered for ID '" + id + "'."); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return result;
	}

	/**
	 * <p>
	 * used by the plugin to register compilers that are declared in the
	 * plugin.xml with the CompilerManager.
	 * </p>
	 */
	@Override
  public void registerCompiler(final String id, final IConfigurationElement info) {
		htRegisteredCompilers.put(id, info);
	}

	/**
	 * <p>
	 * returns the human-readable name for the compiler with the specified id
	 * (if it is a registered compiler).
	 * </p>
	 */
	@Override
  public String getCompilerName( final String id ) {
    String result;
    if( id.equals( DEFAULT ) ) {
      result = CoreTexts.compilerManager_noNamePlaceHolder;
    } else {
      IConfigurationElement elem = htRegisteredCompilers.get( id );
      result = elem.getAttribute( ATT_NAME );
    }
    return result;
  }

	@Override
  public IHsImplementation getCurrentHsImplementation() {
	  return currentHsImplementation;
	}


	// helping methods
	// ////////////////

	private void initDefaultCompiler() {
		installCompiler(DEFAULT, new DefaultHaskellCompiler());
	}

	private void installCompiler(final String id) throws HaskellCoreException {
		IConfigurationElement configElem = htRegisteredCompilers.get(id);
		installCompilerExecutable(id, configElem);
	}

	private void installCompilerExecutable(final String id,
			final IConfigurationElement elem) throws HaskellCoreException {
		Object compiler = null;
		try {
			compiler = elem.createExecutableExtension(HaskellCorePlugin.ATT_CLASS);
		} catch (CoreException cex) {
			fireHCEx(cex.getMessage());
		}
		if (!(compiler instanceof IHaskellCompiler)) {
			fireHCEx("Putative Haskell compiler '" + id + "' must implement" //$NON-NLS-1$ //$NON-NLS-2$
					+ IHaskellCompiler.class.getName());
		}
		IHaskellCompiler haskellCompiler = (IHaskellCompiler) compiler;
		installCompiler(id, haskellCompiler);
	}

	@Override
  public void installCompiler(final String id,
			final IHaskellCompiler haskellCompiler) {
		htInstalledCompilers.put(id, new ListenableCompilerDecorator(
				haskellCompiler));
	}

	private void fireHCEx(final String message) throws HaskellCoreException {
		throw new HaskellCoreException(message);
	}

	@Override
  public void addCompilerListener(final ICompilerListener listener) {
		getSelectedCompilerDecorator().addListener(listener);
	}

	@Override
  public void removeCompilerListener(final ICompilerListener listener) {
		getSelectedCompilerDecorator().removeListener(listener);
	}

	private ListenableCompilerDecorator getSelectedCompilerDecorator() {
		return htInstalledCompilers.get(selectedCompiler);
	}

  private void initHsImplementation() {
    IPreferencesService prefSvc = Platform.getPreferencesService();
    String currentImplName = prefSvc.getString( HaskellCorePlugin.getPluginId(), ICorePreferenceNames.SELECTED_HS_IMPLEMENTATION, null, null );
    if( currentImplName != null ) {
      Map<String, IHsImplementation> impls = loadImpls();
      this.currentHsImplementation = impls.get( currentImplName );
    } else {
      List<IHsImplementation> impls=autodetectGHCImpls();
      if (impls.size()>0){
        setHsImplementations(impls,impls.get( 0 ));
        initHsImplementation() ;
      }
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

  private Map<String, IHsImplementation> loadImpls() {
    List<IHsImplementation> impls = new ArrayList<IHsImplementation>();
    String xml = Platform.getPreferencesService().getString( HaskellCorePlugin.getPluginId(), ICorePreferenceNames.HS_IMPLEMENTATIONS, null, null );
    HsImplementationPersister.fromXML( xml, impls );
    Map<String, IHsImplementation> result = new HashMap<String, IHsImplementation>();
    for( IHsImplementation impl: impls ) {
      result.put( impl.getName(), impl );
    }
    return result;
  }

  private void listenForImplPref() {
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
          IStatus[] statuss=impl.validate();
          boolean err=false;
          for( int i = 0; i < statuss.length; i++ ) {
            IStatus curr = statuss[ i ];
            if( curr.matches( IStatus.ERROR ) ) {
              err=true;
            }
          }
          if (!err){
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