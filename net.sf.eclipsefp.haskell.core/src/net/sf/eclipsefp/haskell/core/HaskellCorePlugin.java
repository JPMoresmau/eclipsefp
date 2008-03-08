// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core;

import java.util.Iterator;
import java.util.List;
import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;
import net.sf.eclipsefp.haskell.core.expressions.HaskellPropertyTester;
import net.sf.eclipsefp.haskell.core.halamo.HaskellModelManager;
import net.sf.eclipsefp.haskell.core.parser.ParserManager;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 *
 * @author The mighty PDE wizard
 */
public class HaskellCorePlugin extends Plugin {

  // extension points
	public static final String ID_EXT_POINT_COMPILERS = "haskellCompilers"; //$NON-NLS-1$
	public static final String ID_EXT_POINT_PARSERS = "haskellParsers"; //$NON-NLS-1$
	public static final String ID_PROBLEM_MARKER = "net.sf.eclipsefp.haskell.core.problem"; //$NON-NLS-1$
	public static final String ID_PROJECT_PROBLEM_MARKER = "net.sf.eclipsefp.haskell.core.projectProblem"; //$NON-NLS-1$

	private static final String ATT_ID = "id"; //$NON-NLS-1$

	private static HaskellCorePlugin plugin;

	private final IWorkspace fWorkspace;
	private HaskellModelManager fModelManager;

	public HaskellCorePlugin() {
		this(ResourcesPlugin.getWorkspace());
	}

	public HaskellCorePlugin(final IWorkspace workspace) {
		plugin = this;
		fWorkspace = workspace;
	}

	@Override
	public void start(final BundleContext context) throws Exception {
		super.start(context);

		collectCompilerInfo();
		collectParserInfo();
		try {
			fModelManager = new HaskellModelManager(fWorkspace);
			fModelManager.initialize();
		} catch (CoreException ex) {
			String message = "Serious problem: could not initialize the Haskell " //$NON-NLS-1$
					+ "language model."; //$NON-NLS-1$
			HaskellCorePlugin.log(message, ex);
		}

		Platform.getAdapterManager()
			.registerAdapters(new HaskellPropertyTester(), IResource.class);
	}

	public HaskellModelManager getModelManager() {
		return fModelManager;
	}

	/**
	 * Returns the shared instance's model manager
	 */
	public static HaskellModelManager getDefaultModelManager() {
		return getDefault().getModelManager();
	}
	/**
	 * <p>
	 * returns the shared instance.
	 * </p>
	 */
	public static HaskellCorePlugin getDefault() {
		return plugin;
	}

	public static String getPluginId() {
		return getDefault().getBundle().getSymbolicName();
	}


	// logging and tracing
	// ////////////////////

	/**
	 * Logs a message to the plugin's error log
	 *
	 * @param message	Message to be logger
	 * @param severity	One of IStatus.ERROR, IStatus.CANCEL, IStatus.INFO,
	 * 					IStatus.OK or IStatus.WARNING
	 */
	public static void log( final String message, final int severity ) {
    String id = getPluginId();
    Status status = new Status( severity, id, IStatus.OK, message, null );
    getDefault().getLog().log( status );
  }

  public static void log( final String message, final Throwable thr ) {
    String id = getPluginId();
    Status status = new Status( IStatus.ERROR, id, IStatus.OK, message, thr );
    getDefault().getLog().log( status );
  }

  public static void log( final Throwable thr ) {
    String msg = thr.getMessage() == null ? "[No detail]" : thr.getMessage(); //$NON-NLS-1$
    log( msg, thr );
  }

  public static boolean isTracing( final String optionId ) {
    String option = getPluginId() + "/" + optionId; //$NON-NLS-1$
    String value = Platform.getDebugOption( option );
    return value != null && value.trim().equalsIgnoreCase( "true" ); //$NON-NLS-1$
  }

	public static void dump( final List<String> cmdLine ) {
    StringBuffer sb = new StringBuffer();
    Iterator<String> iter = cmdLine.iterator();
    while( iter.hasNext() ) {
      sb.append( iter.next() );
      sb.append( " " ); //$NON-NLS-1$
    }
    System.out.println( sb.toString() );
  }


	// helping methods
	// ////////////////

	/**
	 * reads compiler infos out of the extensions declared in the manifest and
	 * registers them with the compiler manager.
	 *
	 * All compiler management is delegated to the CompilerManager singleton.
	 */
	private void collectCompilerInfo() {
    IConfigurationElement[] elements = getExtensions( ID_EXT_POINT_COMPILERS );
    for( int i = 0; i < elements.length; i++ ) {
      String compilerId = elements[ i ].getAttribute( ATT_ID );
      CompilerManager.getInstance()
          .registerCompiler( compilerId, elements[ i ] );
    }
    String pref = ""; //$NON-NLS-1$
    try {
      String name = ICorePreferenceNames.SELECTED_COMPILER;
      pref = getPluginPreferences().getString( name );
      CompilerManager.getInstance().selectCompiler( pref );
    } catch( Exception ex ) {
      String msg = "Problem when selecting compiler '" + pref + "'."; //$NON-NLS-1$ //$NON-NLS-2$
      HaskellCorePlugin.log( msg, ex );
    }
  }

	private void collectParserInfo() {
		IConfigurationElement[] elements = getExtensions(ID_EXT_POINT_PARSERS);
		for (int i = 0; i < elements.length; i++) {
			try {
				String parserId = elements[i].getAttribute(ATT_ID);
				if (parserId == null) {
					String msg = "Haskell parser declaration is missing id attribute " //$NON-NLS-1$
							+ "and will be ignored."; //$NON-NLS-1$
					log(msg, null);
				} else {
					ParserManager.getInstance().registerParser(parserId,
							elements[i]);
				}
			} catch (final CoreException ex) {
				getLog().log(ex.getStatus());
			}
		}
	}

	private IConfigurationElement[] getExtensions(final String key) {
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		return registry.getConfigurationElementsFor(getPluginId(), key);
	}
}