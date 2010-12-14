// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;

/**
 * Computes the content assist completion proposals and context information.
 *
 * @author Leif Frenzel
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public class HaskellContentAssistProcessor implements IContentAssistProcessor {
  /** Default constructor */
	public HaskellContentAssistProcessor() {
	  super();
	}

	// =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
  // interface methods of IContentAssistProcessor
  // =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

	public ICompletionProposal[] computeCompletionProposals(final ITextViewer viewer, final int offset)
	{
		IHaskellCompletionContext context = new WorkbenchHaskellCompletionContext( viewer, offset );
		HSCodeTemplateAssistProcessor templates = new HSCodeTemplateAssistProcessor();
		ICompletionProposal[] contextProposals = context.computeProposals();
		ICompletionProposal[] templateProposals = templates.computeCompletionProposals( viewer, offset );

		// Merge the results together (templates precede generated proposals):
		int totalSize = contextProposals.length + templateProposals.length;
    int endIndex = 0;
		ICompletionProposal[] result = new ICompletionProposal[ totalSize ];

		if ( templateProposals.length > 0 ) {
		  System.arraycopy( templateProposals, 0, result, endIndex, templateProposals.length );
		  endIndex += templateProposals.length;
		}

		if ( contextProposals.length > 0 ) {
		  System.arraycopy( contextProposals, 0, result, endIndex, contextProposals.length );
		}

		return (totalSize > 0 ? result : null);
	}

	public IContextInformation[] computeContextInformation(final ITextViewer viewer, final int documentOffset) {
		// unused
		return null;
	}

	public char[] getCompletionProposalAutoActivationCharacters() {
	  // unused
		return null;
	}

	public char[] getContextInformationAutoActivationCharacters() {
	  // unused
		return null;
	}

	public String getErrorMessage() {
		// return null to indicate we had no problems
		return null;
	}

	public IContextInformationValidator getContextInformationValidator() {
	  // unused
		return null;
	}
}