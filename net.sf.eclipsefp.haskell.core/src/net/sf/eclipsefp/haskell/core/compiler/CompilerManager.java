// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.compiler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.core.HaskellCoreException;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.util.CoreTexts;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.core.runtime.preferences.InstanceScope;

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
      String msg =   "No valid Haskell implementation found. Possibly none " //$NON-NLS-1$
                   + "was configured in the Haskell preferences."; //$NON-NLS-1$
      HaskellCorePlugin.log( msg, IStatus.WARNING );
    }
		listenForImplPref();
	}

  /** Get the CompilerManager instance. */
	public static final ICompilerManager getInstance() {
		return CompilerManagerHolder.theInstance;
	}

	/**
	 * <p>
	 * returns the ids of all compilers registered with the compiler manager.
	 * </p>
	 */
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
	public IHaskellCompiler getCompiler() {
		return htInstalledCompilers.get(selectedCompiler);
	}

	/**
	 * <p>
	 * if the compiler specified by id is known in the compiler manager, it is
	 * selected and will be the one returned by getCompiler() from now on.
	 * </p>
	 */
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
	public void registerCompiler(final String id, final IConfigurationElement info) {
		htRegisteredCompilers.put(id, info);
	}

	/**
	 * <p>
	 * returns the human-readable name for the compiler with the specified id
	 * (if it is a registered compiler).
	 * </p>
	 */
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

	public void installCompiler(final String id,
			final IHaskellCompiler haskellCompiler) {
		htInstalledCompilers.put(id, new ListenableCompilerDecorator(
				haskellCompiler));
	}

	private void fireHCEx(final String message) throws HaskellCoreException {
		throw new HaskellCoreException(message);
	}

	public void addCompilerListener(final ICompilerListener listener) {
		getSelectedCompilerDecorator().addListener(listener);
	}

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
      public void preferenceChange( final PreferenceChangeEvent event ) {
        String key = event.getKey();
        if(    ICorePreferenceNames.HS_IMPLEMENTATIONS.equals( key )
            || ICorePreferenceNames.SELECTED_HS_IMPLEMENTATION.equals( key ) ) {
          initHsImplementation();
        }
      }
    });
  }
}