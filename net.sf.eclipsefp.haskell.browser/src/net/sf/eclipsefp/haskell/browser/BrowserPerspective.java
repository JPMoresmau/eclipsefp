package net.sf.eclipsefp.haskell.browser;

import net.sf.eclipsefp.haskell.browser.views.FunctionsView;
import net.sf.eclipsefp.haskell.browser.views.ModulesView;
import net.sf.eclipsefp.haskell.browser.views.PackagesView;
import net.sf.eclipsefp.haskell.browser.views.TypesView;

import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

public class BrowserPerspective implements IPerspectiveFactory {

	public void createInitialLayout(IPageLayout layout) {
		// Get the editor area
		String editorArea = layout.getEditorArea();
		
		layout.addView(PackagesView.ID, IPageLayout.TOP, 0.5f, editorArea);
		layout.addView(ModulesView.ID, IPageLayout.RIGHT, 0.25f, PackagesView.ID);
		layout.addView(TypesView.ID, IPageLayout.RIGHT, 0.33f, ModulesView.ID);
		layout.addView(FunctionsView.ID, IPageLayout.RIGHT, 0.5f, TypesView.ID);
	}

}
