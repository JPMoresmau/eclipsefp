package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import org.eclipse.jface.text.contentassist.ICompletionProposal;

public interface IHaskellCompletionContext {

	ICompletionProposal[] computeProposals();

}
