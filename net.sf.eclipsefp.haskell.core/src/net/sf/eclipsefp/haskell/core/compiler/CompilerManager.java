// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.compiler;

import java.util.ArrayList;
import java.util.Hashtable;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;

import net.sf.eclipsefp.haskell.core.HaskellCoreException;
import net.sf.eclipsefp.haskell.core.compiler.defaultcompiler.DefaultHaskellCompiler;


/** <p>manages the Haskell compilers.</p>
  * 
  * @author Leif Frenzel
  */
public class CompilerManager {

  /** the singleton instance of CompilerManager. */
  private static CompilerManager _instance;
  
  private static final String DEFAULT = DefaultHaskellCompiler.class.getName();

  /** the currently selected compiler. This is the compiler that is obtained 
    * by builders when they query getCompiler(). After registering a compiler,
    * it has to be selected with selectCompiler. By the default, the 
    * DefaultCompiler (a dummy object that does no compiling itself) is 
    * selected. */
  private String selectedCompiler = DEFAULT;
  /** registered compilers.
    * 
    * keys  - compiler id
    * value - compiler information as provided in the plugin.xml 
    */ 
  private Hashtable htRegisteredCompilers;
  /** installed compilers in the compiler manager. A compiler is installed
    * (objects are created, inits are performed) only if it is used. Then it
    * is cached here.
    * 
    * key   - compiler id
    * value - the compiler object (which implements IHaskellCompiler)
    */
  private Hashtable htInstalledCompilers;

  /** contains ICompilerOutputListeners that have registered with the
    * compiler manager to get informed about output produced by compilers. */
  private ArrayList alListeners;
  
  /** creates the singleton instance of CompilerManager. Private in order
    * to ensure the singleton pattern. */ 
  private CompilerManager() {
    htInstalledCompilers = new Hashtable();
    htRegisteredCompilers = new Hashtable();
    alListeners = new ArrayList();
    initDefaultCompiler();
  }
  
  /** <p>returns a reference to the singleton instance of 
    * CompilerManager.</p> */
  public static synchronized CompilerManager getInstance() {
    if( _instance == null ) {
      _instance = new CompilerManager();
    }
    return _instance;
  }
  
  /** <p>returns the ids of all compilers registered with the compiler 
    * manager.</p> */
  public String[] getRegisteredCompilers() {
    int size = htRegisteredCompilers.size();
    String[] result = new String[ size + 1 ];
    htRegisteredCompilers.keySet().toArray( result );
    result[ size ] = DEFAULT;
    return result;
  }
  
  /** <p>returns the currently used Haskell compiler.</p> */
  public IHaskellCompiler getCompiler() {
    return ( IHaskellCompiler )htInstalledCompilers.get( selectedCompiler );
  }
  
  /** <p>if the compiler specified by id is known in the compiler manager,
    * it is selected and will be the one returned by getCompiler() from
    * now on.</p> */
  public boolean selectCompiler( final String id ) throws Exception {
    boolean result = false;
    if( htInstalledCompilers.containsKey( id ) ) {
      selectedCompiler = id;
      result = true;
    } else if( htRegisteredCompilers.containsKey( id ) ) {
      try {
        installCompiler( id );
      } catch( HaskellCoreException hcEex ) {
        throw new Exception( "Could not install compiler '" + id + "'.\n", 
                             hcEex );
      }
      selectedCompiler = id;
      result = true;
    } else {
      fireHCEx( "No Haskell compiler registered for ID '" + id + "'." );
    }
    return result;
  }
  
  /** <p>used by the plugin to register compilers that are declared in the 
    * plugin.xml with the CompilerManager.</p> */
  public void registerCompiler( final String id, 
                                final IConfigurationElement info )  {
    htRegisteredCompilers.put( id, info );
  }

  /** <p>registers the passed listener with the compiler manager. The 
    * listener is notified about compiler output that has been produced
    * when the currently selected compiler compiled.</p> */
  public void addCompilerOutputListener( final ICompilerOutputListener li ) {
    alListeners.add( li );
  }

  /** <p>removes the passed listener from the list of listeners that are
    * notifed about compiler output.</p> */
  public void removeCompilerOutputListener( final ICompilerOutputListener li ) {
    alListeners.remove( li );
  }

  /** <p>notifies all compiler output listeners that have registered with the
    * CompilerManager that the passed output has been produced.</p> */
  public void notifyListeners( final ICompilerOutput output ) {
    for( int i = 0; i < alListeners.size(); i++ ) {
      Object obj = alListeners.get( i );
      ICompilerOutputListener li = ( ICompilerOutputListener )obj;
      li.outputProduced( output );
    }
  }
  
  /** <p>returns the human-readable name for the compiler with the specified
    * id (if it is a registered compiler).</p> */
  public String getCompilerName( final String id ) {
    String result;
    if( id.equals( DEFAULT ) ) {
      result = "No compiler";
    } else {
      IConfigurationElement elem 
        = ( IConfigurationElement )htRegisteredCompilers.get( id );
      result = elem.getAttribute( "name" ); 
    }
    return result;
  }
  
  
  // helping methods
  //////////////////
  
  private void initDefaultCompiler() {
    IHaskellCompiler defaultCompiler = new DefaultHaskellCompiler();
    htInstalledCompilers.put( DEFAULT, defaultCompiler );
  }
  
  private void installCompiler( final String id ) throws HaskellCoreException {
    IConfigurationElement configElem 
      = ( IConfigurationElement )htRegisteredCompilers.get( id );
    installCompilerExecutable( id, configElem );
  }

  private void installCompilerExecutable( final String id,
                                          final IConfigurationElement elem )
                                                   throws HaskellCoreException {
    Object compiler = null;
    try {
      compiler = elem.createExecutableExtension( "class" );
    } catch ( CoreException cex ) {
      fireHCEx( cex.getMessage() );
    }
    if( !( compiler instanceof IHaskellCompiler ) ) {
      fireHCEx(   "Putative Haskell compiler '" 
                + id 
                + "' must implement" 
                + IHaskellCompiler.class.getName() );
    }
    htInstalledCompilers.put( id, compiler );
  }

  private void fireHCEx( final String message ) throws HaskellCoreException {
    throw new HaskellCoreException( message );
  }
}