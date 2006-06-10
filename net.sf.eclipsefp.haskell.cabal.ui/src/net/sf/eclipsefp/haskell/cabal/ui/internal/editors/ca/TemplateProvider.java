// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.ui.internal.editors.ca;

import java.io.IOException;

import net.sf.eclipsefp.haskell.cabal.ui.internal.CabalUIPlugin;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.templates.ContextTypeRegistry;
import org.eclipse.jface.text.templates.persistence.TemplateStore;
import org.eclipse.ui.editors.text.templates.ContributionContextTypeRegistry;
import org.eclipse.ui.editors.text.templates.ContributionTemplateStore;

/** <p>entry point for access to contributed code assist templates.</p> 
  *
  * @author Leif Frenzel
  */
public class TemplateProvider {

  private static ContributionContextTypeRegistry contextTypeRegistry;
  private static TemplateStore templateStore;
  
  public static TemplateStore getTemplateStore() {
    if( templateStore == null ) {
      IPreferenceStore prefs = CabalUIPlugin.getDefault().getPreferenceStore();
      templateStore = new ContributionTemplateStore( getContextTypeRegistry(), 
                                                     prefs, 
                                                     "templates" );
      try {
        templateStore.load();
      } catch( final IOException ioex ) {
        CabalUIPlugin.log( "Unable to load templates.", ioex );
      }
    }

    return templateStore;
  }

  public static ContextTypeRegistry getContextTypeRegistry() {
    if( contextTypeRegistry == null ) {
      contextTypeRegistry = new ContributionContextTypeRegistry();
      contextTypeRegistry.addContextType( "cabalPackageDescriptionTemplates" );
    }
    return contextTypeRegistry;
  }
}
