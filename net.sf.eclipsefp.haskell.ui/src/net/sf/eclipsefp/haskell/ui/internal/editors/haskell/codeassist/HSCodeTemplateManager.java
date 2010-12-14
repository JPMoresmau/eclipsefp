package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import java.io.IOException;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.templates.ContextTypeRegistry;
import org.eclipse.jface.text.templates.persistence.TemplateStore;
import org.eclipse.ui.editors.text.templates.ContributionContextTypeRegistry;
import org.eclipse.ui.editors.text.templates.ContributionTemplateStore;
import org.osgi.service.prefs.BackingStoreException;

/**
 * The Haskell source code template manager. This class is a singleton instance that loads the
 * default and user source code templates from the UI's preference store, and manages the template
 * context types.
  *
  * @author B. Scott Michel
 */
public class HSCodeTemplateManager {
  /** The preference key where the user's custom templates are stored. */
  private static final String CUSTOM_TEMPLATES_KEY  = HaskellUIPlugin.getPluginId() + ".customtemplates";
  /** The singleton instance holder class */
  private static class HSCodeTemplateManagerHolder {
    /** The lazily constructed singleton instance */
    private static final HSCodeTemplateManager instance = new HSCodeTemplateManager();
  }

  /** The template store; contains the templates read from the default and custom user preferences */
  private TemplateStore fStore;
  /** The template context registry */
  private final ContributionContextTypeRegistry fRegistry;
  /* Unused: private TemplatePersistenceData[] templateData; */

  /** Internal constructor, only referenced by the singleton instance holder. */
  private HSCodeTemplateManager() {
    fRegistry = new ContributionContextTypeRegistry();
  }

  /** Get the singleton instance of the code template manager */
  public static HSCodeTemplateManager getInstance() {
    return HSCodeTemplateManagerHolder.instance;
  }

  /** Get the template store, reading the templates if necessary */
  public TemplateStore getTemplateStore() {
    if( fStore == null ) {
      fStore = new ContributionTemplateStore( getContextTypeRegistry(), getPreferenceStore(), CUSTOM_TEMPLATES_KEY );
      try {
        fStore.load();
      } catch( IOException e ) {
        e.printStackTrace();
      }
    }

    return fStore;
  }

  /** Get the template context registry, initializing the registry container if necessary */
  public ContextTypeRegistry getContextTypeRegistry() {
    fRegistry.addContextType( HSCodeTemplateContextType.CONTEXT_TYPE );
    return fRegistry;
  }

  /** Get the preference store */
  public IPreferenceStore getPreferenceStore() {
    return HaskellUIPlugin.getDefault().getPreferenceStore();
  }

  /** Save the template's preferences: */
  public void savePluginPreferences() {
    try {
      new InstanceScope().getNode(CUSTOM_TEMPLATES_KEY).flush();
    } catch( BackingStoreException ex ) {
      HaskellUIPlugin.log( UITexts.template_prefSave_backingStore_exception, ex );
    }
  }
}
