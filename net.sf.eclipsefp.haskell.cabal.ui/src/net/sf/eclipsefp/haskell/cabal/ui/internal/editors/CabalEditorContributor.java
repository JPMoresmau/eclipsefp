// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.ui.internal.editors;

import java.util.ResourceBundle;

import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.texteditor.BasicTextEditorActionContributor;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.eclipse.ui.texteditor.RetargetTextEditorAction;

/** <p>the action contribution for the Cabal editor.</p> 
  * 
  * <p>Note: this class is declared in <code>plugin.xml</code>.</p>
  *
  * @author Leif Frenzel
  */
public class CabalEditorContributor extends BasicTextEditorActionContributor {

	private static final String CONTENTASSIST_ACTION 
	  = "org.eclipse.ui.examples.recipeeditor.ContentAssist"; //$NON-NLS-1$
	
	private final RetargetTextEditorAction action;
	
	public CabalEditorContributor() {
		ResourceBundle rb = ResourceBundle.getBundle( CabalEditor.class.getName() );
    action = new RetargetTextEditorAction( rb, "ContentAssistProposal." ); //$NON-NLS-1$
		String adi = ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS;
    action.setActionDefinitionId( adi ); 
	}
	
	@Override
	public void setActiveEditor( final IEditorPart part ) {
		super.setActiveEditor( part );
		ITextEditor editor = ( part instanceof ITextEditor ) ? ( ITextEditor )part 
                                                         : null;
		action.setAction( getAction( editor, CONTENTASSIST_ACTION ) );
	}
	
	@Override
	public void init( final IActionBars bars, final IWorkbenchPage page ) {
		super.init( bars, page );
		bars.setGlobalActionHandler( CONTENTASSIST_ACTION, action );
	}

	@Override
	public void dispose() {
		setActiveEditor( null );
		super.dispose();
	}
}
