package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import net.sf.eclipsefp.haskell.ui.actions.OpenDefinitionAction;

import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.editors.text.TextEditorActionContributor;

public class HaskellEditorActionContributor extends TextEditorActionContributor {

	private final OpenDefinitionAction openDefinitionAction;

	public HaskellEditorActionContributor() {
		openDefinitionAction = new OpenDefinitionAction();
		openDefinitionAction.setActionDefinitionId(IActionDefinitionIds.OPEN_DEFINITION);
	}

	@Override
	public void init(final IActionBars bars, final IWorkbenchPage page) {
		super.init(bars, page);
		bars.setGlobalActionHandler(IActionDefinitionIds.OPEN_DEFINITION, openDefinitionAction);
	}

	@Override
	public void setActiveEditor(final IEditorPart part) {
		super.setActiveEditor(part);
		if (part instanceof HaskellEditor) {
			openDefinitionAction.setCurrentEditor((HaskellEditor)part);
		}
	}

}
