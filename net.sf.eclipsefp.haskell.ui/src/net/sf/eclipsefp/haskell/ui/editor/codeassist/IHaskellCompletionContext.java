package net.sf.eclipsefp.haskell.ui.editor.codeassist;

import org.eclipse.jface.text.contentassist.ICompletionProposal;

public interface IHaskellCompletionContext {

	ICompletionProposal[] computeProposals();
	
}
