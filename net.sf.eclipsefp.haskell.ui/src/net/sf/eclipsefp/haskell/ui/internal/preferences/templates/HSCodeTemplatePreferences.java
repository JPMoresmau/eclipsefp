package net.sf.eclipsefp.haskell.ui.internal.preferences.templates;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist.HSCodeTemplateManager;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.texteditor.templates.TemplatePreferencePage;

/**
 * This class represents a preference page that
 * is contributed to the Preferences dialog. By
 * subclassing <samp>FieldEditorPreferencePage</samp>, we
 * can use the field support built into JFace that allows
 * us to create a page that is small and knows how to
 * save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They
 * are stored in the preference store that belongs to
 * the main plug-in class. That way, preferences can
 * be accessed directly via the preference store.
 */

public class HSCodeTemplatePreferences extends TemplatePreferencePage implements IWorkbenchPreferencePage {

	public HSCodeTemplatePreferences() {
		super();
		setPreferenceStore(HaskellUIPlugin.getDefault().getPreferenceStore());
		setTemplateStore(HSCodeTemplateManager.getInstance().getTemplateStore());
		setContextTypeRegistry(HSCodeTemplateManager.getInstance().getContextTypeRegistry());
	}

	@Override
	protected boolean isShowFormatterSetting() {
	  return false;
	}

	@Override
	public boolean performOk() {
	  boolean ok = super.performOk();
	  HaskellUIPlugin.getDefault().savePluginPreferences();
	  return ok;
	}
}