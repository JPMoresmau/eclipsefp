/*******************************************************************************
 * Copyright (c) 2003-2005, Leif Frenzel and others. See http://leiffrenzel.de
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Leif Frenzel - Initial API and implementation
 *     Thiago Arrais - Preference controls
 *******************************************************************************/
package net.sf.eclipsefp.haskell.ui;

import java.util.MissingResourceException;
import java.util.ResourceBundle;
import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.ColorProvider;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.ScannerManager;
import net.sf.eclipsefp.haskell.ui.internal.preferences.HaskellPreferenceManager;
import net.sf.eclipsefp.haskell.ui.internal.scion.ScionManager;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * <p>
 * The main plugin class to be used in the desktop.
 * </p>
 *
 * @author the mighty PDE wizard
 */
public class HaskellUIPlugin extends AbstractUIPlugin {

  public static final String ID_PERSPECTIVE = HaskellPerspective.class
      .getName();

  // The shared instance.
  private static HaskellUIPlugin plugin;
  // Resource bundle.
  private ResourceBundle resourceBundle;

  private HaskellPreferenceManager fPreferenceManager;

  private ScionManager fScionManager = null;

  public HaskellUIPlugin() {
    plugin = this;
    try {
      String name = HaskellUIPlugin.class.getName();
      resourceBundle = ResourceBundle.getBundle( name );
    } catch( MissingResourceException x ) {
      resourceBundle = null;
    }
  }

  @Override
  public void stop( final BundleContext context ) throws Exception {
    fScionManager.stop();
    HaskellUIImages.disposeImageDescriptorRegistry();
    ColorProvider.getInstance().dispose();
    ScannerManager.getInstance().dispose();
    super.stop( context );
  }

  @Override
  public void start( final BundleContext context ) throws Exception {
    super.start( context );
    getPreferenceManager().activateBuildConsolePreferences();
    fScionManager = new ScionManager();
    fScionManager.start();
  }

  public HaskellPreferenceManager getPreferenceManager() {
    if( null == fPreferenceManager ) {
      fPreferenceManager = new HaskellPreferenceManager( CompilerManager
          .getInstance(), getPreferenceStore() );
    }
    return fPreferenceManager;
  }

  /**
   * <p>
   * returns the shared instance.
   * </p>
   */
  public static HaskellUIPlugin getDefault() {
    return plugin;
  }

  /**
   * <p>
   * returns the unique identifier for the HaskellUIPlugin.
   * </p>
   */
  public static String getPluginId() {
    return getDefault().getBundle().getSymbolicName();
  }

  /**
   * <p>
   * returns the standard display to be used.
   * </p>
   * <p>
   * The method first checks, if the thread calling this method has an
   * associated display. If so, this display is returned. Otherwise the method
   * returns the default display.
   * </p>
   */
  public static Display getStandardDisplay() {
    Display display = Display.getCurrent();
    if( display == null ) {
      display = Display.getDefault();
    }
    return display;
  }

  @Override
  protected ImageRegistry createImageRegistry() {
    return HaskellUIImages.initializeImageRegistry();
  }

  // resource bundle management
  // ///////////////////////////

  /**
   * <p>
   * returns the plugin's resource bundle.
   * </p>
   */
  public ResourceBundle getResourceBundle() {
    return resourceBundle;
  }

  // logging
  // ////////

  public static void log( final String message, final int severity ) {
    logg( message, severity, null );
  }

  public static void log( final Throwable throwable ) {
    logg( throwable.getMessage(), IStatus.ERROR, throwable );
  }

  public static void log( final String message, final Throwable thr ) {
    logg( message, IStatus.ERROR, thr );
  }

  // Scion manager
  // //////////////

  public ScionManager getScionManager() {
    return fScionManager;
  }

  public ScionInstance getScionInstanceManager( final IResource resource ) {
    return fScionManager.getScionInstance( resource );
  }

  // helping methods
  // ////////////////

  private static void logg( final String message, final int severity,
      final Throwable thr ) {
    String msg = message == null ? "[No details]" : message;
    Status status = new Status( severity, getPluginId(), IStatus.OK, msg, thr );
    getDefault().getLog().log( status );
  }
}