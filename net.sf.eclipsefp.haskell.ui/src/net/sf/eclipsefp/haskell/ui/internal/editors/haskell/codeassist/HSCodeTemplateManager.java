package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import java.io.IOException;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.templates.ContextTypeRegistry;
import org.eclipse.jface.text.templates.persistence.TemplatePersistenceData;
import org.eclipse.jface.text.templates.persistence.TemplateStore;
import org.eclipse.ui.editors.text.templates.ContributionContextTypeRegistry;
import org.eclipse.ui.editors.text.templates.ContributionTemplateStore;


public class HSCodeTemplateManager {
  private static final String CUSTOM_TEMPLATES_KEY  = HaskellUIPlugin.getPluginId() + ".customtemplates";

  private static class HSCodeTemplateManagerHolder {
    private static final HSCodeTemplateManager instance = new HSCodeTemplateManager();
  }

  private TemplateStore fStore;
  private ContributionContextTypeRegistry fRegistry;
  private TemplatePersistenceData[] templateData;

  private HSCodeTemplateManager() {
    // NOP
  }

  public static HSCodeTemplateManager getInstance() {
    return HSCodeTemplateManagerHolder.instance;
  }

  public TemplateStore getTemplateStore() {
    if( fStore == null ) {
      fStore = new ContributionTemplateStore( getContextTypeRegistry(), HaskellUIPlugin.getDefault().getPreferenceStore(),
                                              CUSTOM_TEMPLATES_KEY );
      try {
        fStore.load();
      } catch( IOException e ) {
        e.printStackTrace();
      }
    }

    return fStore;
  }


  public ContextTypeRegistry getContextTypeRegistry() {
    if( fRegistry == null ) {
      fRegistry = new ContributionContextTypeRegistry();
    }
    fRegistry.addContextType( HSCodeTemplateContextType.CONTEXT_TYPE );
    return fRegistry;
  }

  public IPreferenceStore getPreferenceStore() {
    return HaskellUIPlugin.getDefault().getPreferenceStore();
  }

  public void savePluginPreferences() {
    HaskellUIPlugin.getDefault().savePluginPreferences();
  }
}
