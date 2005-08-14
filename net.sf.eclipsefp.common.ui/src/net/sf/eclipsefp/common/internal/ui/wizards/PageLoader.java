// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.internal.ui.wizards;

import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.common.ui.CommonUIPlugin;
import net.sf.eclipsefp.common.ui.configurator.IConfiguratorPage;
import net.sf.eclipsefp.common.ui.configurator.IProbe;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.ui.plugin.AbstractUIPlugin;

/** <p>provides helping functionality for creating pages for the 
  * configurator wizard. Contributions are collected from the registry and
  * wizard pages are created for them.</p>
  *
  * @author Leif Frenzel
  */
class PageLoader {

  private static final String ELEM_CONFIGURATOR_PAGE = "configuratorPage";
  private static final String ATT_CLASS              = "class";
  private static final String ATT_PROBE_CLASS        = "probeClass";
  private static final String ATT_ID                 = "id";
  private static final String ATT_TITLE              = "title";
  private static final String ATT_DESCRIPTION        = "description";
  private static final String ATT_BANNER_IMAGE       = "bannerImage";
  
   
  static IWizardPage[] createPages() {
    List list = new ArrayList();
    System.out.println();
    IConfigurationElement[] elems = collectContributions();
    for( int i = 0; i < elems.length; i++ ) {
      load( elems[ i ], list );
    }
    
    IWizardPage[] result = new IWizardPage[ list.size() ];
    list.toArray( result );
    return result;
  }


  // helping methods
  //////////////////
  
  private static IConfigurationElement[] collectContributions() {
    IExtensionRegistry registry = Platform.getExtensionRegistry();
    String pluginId = CommonUIPlugin.getPluginId();
    String extPointId = CommonUIPlugin.EXT_POINT_CONFIGURATOR_PAGES;
    return registry.getConfigurationElementsFor( pluginId, extPointId );
  }

  private static void load( final IConfigurationElement element, 
                            final List list ) {
    if( isOk( element ) ) {
      final IConfiguratorPage cfgPage = loadConfiguratorPage( element );
      if( cfgPage != null ) {
        String pageId = getAttribute( element, ATT_ID );
        final IProbe probe = loadProbe( element );
        WizardPage page = new ConfiguratorWizardPage( pageId, cfgPage, probe );
        ImageDescriptor imgDesc = getImgDesc( element );
        if( imgDesc != null ) {
          page.setImageDescriptor( imgDesc );
        }
        page.setTitle( getAttribute( element, ATT_TITLE ) );
        page.setDescription( getAttribute( element, ATT_DESCRIPTION ) );
        list.add( page );
      }
    }
  }

  private static ImageDescriptor getImgDesc( final IConfigurationElement ce ) {
    String imgName = ce.getAttribute( ATT_BANNER_IMAGE );
    String pluginId = ce.getDeclaringExtension().getNamespace();
    ImageDescriptor result = null;
    if( imgName != null && !imgName.equals( "" ) ) {
      result = AbstractUIPlugin.imageDescriptorFromPlugin( pluginId, imgName );
    }
    return result;
  }
  
  private static String getAttribute( final IConfigurationElement element, 
                                        final String attribute ) {
    String result = element.getAttribute( attribute );
    if( result == null ) {
      result = "";
    }
    return result;
  }
  
  private static IConfiguratorPage loadConfiguratorPage( 
                                         final IConfigurationElement element ) {
    IConfiguratorPage result = null;
    try {
      Object obj = element.createExecutableExtension( ATT_CLASS );
      if( obj != null && obj instanceof IConfiguratorPage ) {
        result = ( IConfiguratorPage )obj;
      } else {
        log( element, "configuratorPage", null );
      }
    } catch( CoreException cex ) {
      log( element, "configuratorPage", cex );
    }
    return result;
  }

  private static IProbe loadProbe( final IConfigurationElement element ) {
    IProbe result = null;
    try {
      String att = element.getAttribute( ATT_PROBE_CLASS );
      if( att != null && att.trim().length() > 0 ) {
        Object obj = element.createExecutableExtension( ATT_PROBE_CLASS );
        if( obj != null && obj instanceof IProbe ) {
          result = ( IProbe )obj;
        } else {
          log( element, "probe", null );
        }
      }
    } catch( CoreException cex ) {
      log( element, "probe", cex );
    }
    return result;
  }

  private static void log( final IConfigurationElement element, 
                           final String what,
                           final CoreException cex ) {
    String message =   "Could not load " + what + " from plugin " 
                     + element.getDeclaringExtension().getNamespace()
                     + " with id "
                     + element.getAttribute( ATT_ID );
    CommonUIPlugin.log( message, cex );
  }

  private static boolean isOk( final IConfigurationElement element ) {
    return    element != null 
           && element.getName().equals( ELEM_CONFIGURATOR_PAGE );
  }
}
