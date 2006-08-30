// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.*;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;
import net.sf.eclipsefp.haskell.core.halamo.ICompilationUnit;
import net.sf.eclipsefp.haskell.core.halamo.IHaskellLanguageElement;
import net.sf.eclipsefp.haskell.ui.console.CompilerListener;
import net.sf.eclipsefp.haskell.ui.editor.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.editor.text.ColorProvider;
import net.sf.eclipsefp.haskell.ui.editor.text.ScannerManager;
import net.sf.eclipsefp.haskell.ui.preferences.HaskellCompilerPP;
import net.sf.eclipsefp.haskell.ui.preferences.NewHaskellProjectPP;
import net.sf.eclipsefp.haskell.ui.preferences.editor.HaskellEditorPP;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;


/** <p>The main plugin class to be used in the desktop.</p>
  *
  * @author the mighty PDE wizard 
  */
public class HaskellUIPlugin extends AbstractUIPlugin {
  
  public static final String ID_PERSPECTIVE
    = HaskellPerspective.class.getName();  

  //The shared instance.
  private static HaskellUIPlugin plugin;
  //Resource bundle.
  private ResourceBundle resourceBundle;
  
  public HaskellUIPlugin() {
    plugin = this;
    try {
      String name = HaskellUIPlugin.class.getName();
      resourceBundle = ResourceBundle.getBundle( name );
    } catch( MissingResourceException x ) {
      resourceBundle = null;
    }
  }
  
  public void stop( final BundleContext context ) throws Exception {
    HaskellUIImages.disposeImageDescriptorRegistry();
    ColorProvider.getInstance().dispose();
    ScannerManager.getInstance().dispose();
    super.stop( context );
  }
  
  public void start( final BundleContext context ) throws Exception {
	super.start(context);
	CompilerManager.getInstance().addCompilerListener(new CompilerListener());
  }

/** <p>returns the shared instance.</p> */
  public static HaskellUIPlugin getDefault() {
    return plugin;
  }
  
  /** <p>returns the unique identifier for the HaskellUIPlugin.</p> */
  public static String getPluginId() {
    return getDefault().getBundle().getSymbolicName();
  }
  
  /** <p>returns the standard display to be used.</p>
    * <p>The method first checks, if the thread calling this method has an 
    * associated display. If so, this display is returned. Otherwise the 
    * method returns the default display.</p> */
  public static Display getStandardDisplay() {
    Display display = Display.getCurrent();
    if( display == null ) {
      display = Display.getDefault();
    }
    return display;
  }

  protected ImageRegistry createImageRegistry() {
    return HaskellUIImages.initializeImageRegistry();
  }

  // prefs management
  ///////////////////

  protected void initializeDefaultPreferences( final IPreferenceStore store ) {
    HaskellCompilerPP.initializeDefaultValues( store );
    HaskellEditorPP.initializeDefaultValues( store );
    NewHaskellProjectPP.initDefaults( store );
  }


  // resource bundle management
  /////////////////////////////

  /** <p>returns the plugin's resource bundle.</p> */
  public ResourceBundle getResourceBundle() {
    return resourceBundle;
  }
  
  
  // logging
  //////////
  
  public static void log( final String message, final int severity ) {
    Status status = new Status( severity, 
                                getPluginId(), 
                                IStatus.OK, 
                                message, 
                                null );
    getDefault().getLog().log( status );
  }

  public static void log( final String message, final Throwable throwable ) {
    Status status = new Status( IStatus.ERROR, 
                                getPluginId(), 
                                IStatus.OK, 
                                message, 
                                throwable );
    getDefault().getLog().log( status );
  }
  
  
  // convenience methods for the Haskell UI
  /////////////////////////////////////////
  
  public static void showInEditor( final ICompilationUnit compilationUnit ) {
    openEditor( compilationUnit );
  }
  
  public static void showInEditor( final IHaskellLanguageElement element ) {
    ICompilationUnit compilationUnit = element.getCompilationUnit();
    IEditorPart part = openEditor( compilationUnit );
    if( part != null ) {
      ( ( HaskellEditor )part ).reveal( element );
    }
  }

  
  // helping methods
  //////////////////
  
  private static IEditorPart openEditor( final ICompilationUnit cu ) {
    IFile file = cu.getUnderlyingResource();
    IEditorInput input = new FileEditorInput( file );
    IEditorPart part = null;
    try {
      part = getPage().openEditor( input, HaskellEditor.ID, true );
    } catch( PartInitException ex ) {
      log( "Could not open editor for " + cu.toString() + ".", ex );
    }
    return part;
  }

  private static IWorkbenchPage getPage() {
    return PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
  }
}