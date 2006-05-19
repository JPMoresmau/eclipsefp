package net.sf.eclipsefp.haskell.core.codeassist;

import org.eclipse.jface.text.contentassist.ICompletionProposal;

public interface ICompletionEngine {

	ICompletionProposal[] computeProposals(HaskellCompletionContext context);

}
