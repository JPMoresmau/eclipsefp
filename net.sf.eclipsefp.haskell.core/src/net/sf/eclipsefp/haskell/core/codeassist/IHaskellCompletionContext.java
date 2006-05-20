package net.sf.eclipsefp.haskell.core.codeassist;

import org.eclipse.jface.text.contentassist.ICompletionProposal;

public interface IHaskellCompletionContext {

	ICompletionProposal[] computeProposals();
	
}
