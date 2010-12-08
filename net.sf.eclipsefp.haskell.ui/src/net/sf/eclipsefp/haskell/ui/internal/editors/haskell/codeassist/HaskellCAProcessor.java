// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import java.util.ArrayList;
import java.util.List;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;

/**
 * <p>
 * computes the code assist completion proposals and context information.
 * </p>
 *
 * @author Leif Frenzel
 */
public class HaskellCAProcessor implements IContentAssistProcessor {

	public static class NullCompletionContext extends HaskellCompletionContext {

	  static private class NullCompletionContextHolder {
	    private static final NullCompletionContext theInstance = new NullCompletionContext();
	  }

		public static final NullCompletionContext getInstance() {
			return NullCompletionContextHolder.theInstance;
		}

		@Override
		public ICompletionProposal[] computeProposals() {
			return new ICompletionProposal[0];
		}

	}

	private static class WorkbenchContextFactory implements	ICompletionContextFactory {

		public IHaskellCompletionContext createContext(final ITextViewer viewer, final int offset)
		{
				return new WorkbenchHaskellCompletionContext(viewer, offset);
		}
	}

	private final ICompletionContextFactory fContextFactory;

	public HaskellCAProcessor() {
		this(new WorkbenchContextFactory());
	}

	public HaskellCAProcessor(final ICompletionContextFactory factory) {
		fContextFactory = factory;
	}

	// interface methods of IContentAssistProcessor
	// /////////////////////////////////////////////

	public ICompletionProposal[] computeCompletionProposals(final ITextViewer viewer, final int offset)
	{
		IHaskellCompletionContext context = fContextFactory.createContext(viewer, offset);
		HSCodeTemplateAssistProcessor templates = new HSCodeTemplateAssistProcessor();
		ICompletionProposal[] contextProposals = context.computeProposals();
		ICompletionProposal[] templateProposals = templates.computeCompletionProposals( viewer, offset );

		List<ICompletionProposal> proposals = new ArrayList<ICompletionProposal>(contextProposals.length + templateProposals.length);

		for( int i = 0; i < templateProposals.length; i++ ) {
      proposals.add(templateProposals[i]);
    }

		for( int i = 0; i < contextProposals.length; i++ ) {
      proposals.add(contextProposals[i]);
    }

		if (proposals.size() > 0) {
      return proposals.toArray( new ICompletionProposal[ proposals.size() ] );
    }

		return null;
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