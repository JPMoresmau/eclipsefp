// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.ui.internal.util;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import net.sf.eclipsefp.haskell.cabal.ui.internal.CabalUIPlugin;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.osgi.framework.Bundle;

/** <p>contains the image registry for images in the Cabal UI.</p>
  * 
  * @author Leif Frenzel
  */
public class CabalUIImages implements IImageNames {

  private static ImageRegistry imageRegistry;
  
  /** <p>a table of all the <code>ImageDescriptor</code>s.</p> */
  private static Map<String, ImageDescriptor> imageDescriptors;

  private static URL baseUrl;

  static {
    String pathSuffix = "icons/";
    try {
      Bundle bundle = CabalUIPlugin.getDefault().getBundle();
      baseUrl = new URL( bundle.getEntry( "/" ), pathSuffix );
    } catch( MalformedURLException e ) {
      // do nothing
    }
  }

  private final static String OBJECT = "obj16/";   // basic colors - size 16x16


  private static void declareImages() {
    // objects
    declare( EXECUTABLE_STANZA, OBJECT + "executablestanza.gif" );
    declare( LIBRARY_STANZA, OBJECT    + "librarystanza.gif" );
    declare( GENERAL_STANZA, OBJECT    + "generalstanza.gif" );
  }

  private final static void declare( final String key, 
                                                  final String path ) {
    ImageDescriptor desc = ImageDescriptor.getMissingImageDescriptor();
    try {
      desc = ImageDescriptor.createFromURL( makeIconFileURL( path ) );
    } catch( MalformedURLException mux ) {
      CabalUIPlugin.log( "Problem loading image.", mux );
    }
    imageRegistry.put( key, desc );
    imageDescriptors.put( key, desc );
  }
  
  private static ImageRegistry getImageRegistry() {
    if( imageRegistry == null ) {
      initializeImageRegistry();
    }
    return imageRegistry;
  }

  public static ImageRegistry initializeImageRegistry() {
    imageRegistry = new ImageRegistry( Display.getDefault() );
    imageDescriptors = new HashMap<String, ImageDescriptor>( 30 );
    declareImages();
    return imageRegistry;
  }

  /** <p>returns the <code>Image<code> identified by the given key,
    * or <code>null</code> if it does not exist.</p> */
  public static Image getImage( final String key ) {
    return getImageRegistry().get( key );
  }
  
  /** <p>returns the <code>ImageDescriptor<code> identified by the given key,
    * or <code>null</code> if it does not exist.</p> */
  public static ImageDescriptor getImageDescriptor( final String key ) {
    if( imageDescriptors == null ) {
      initializeImageRegistry();
    }
    return ( ImageDescriptor )imageDescriptors.get( key );
  }
  
  private static URL makeIconFileURL( final String iconPath ) 
                                                  throws MalformedURLException {
    if( baseUrl == null ) {
      throw new MalformedURLException();
    }
    return new URL( baseUrl, iconPath );
  }
}