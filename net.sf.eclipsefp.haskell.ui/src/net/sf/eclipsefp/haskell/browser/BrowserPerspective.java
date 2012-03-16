/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser;

import net.sf.eclipsefp.haskell.browser.views.declarations.FunctionsView;
import net.sf.eclipsefp.haskell.browser.views.declarations.TypesView;
import net.sf.eclipsefp.haskell.browser.views.modules.ModulesView;
import net.sf.eclipsefp.haskell.browser.views.packages.PackagesView;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

/**
 * Perspective for Haskell Browser.
 * @author Alejandro Serrano
 */
public class BrowserPerspective implements IPerspectiveFactory {

	@Override
  public void createInitialLayout(final IPageLayout layout) {
		// Get the editor area
		String editorArea = layout.getEditorArea();

		layout.addView(PackagesView.ID, IPageLayout.TOP, 0.5f, editorArea);
		layout.addView(ModulesView.ID, IPageLayout.RIGHT, 0.20f, PackagesView.ID);
		layout.addView(TypesView.ID, IPageLayout.RIGHT, 0.30f, ModulesView.ID);
		layout.addView(FunctionsView.ID, IPageLayout.RIGHT, 0.45f, TypesView.ID);
	}
}
