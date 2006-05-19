// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.editor.codeassist;

import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;

import net.sf.eclipsefp.haskell.core.codeassist.*;

/**
 * <p>
 * computes the code assist completion proposals and context information.
 * </p>
 * 
 * @author Leif Frenzel
 */
public class HaskellCAProcessor implements IContentAssistProcessor {

	private static class WorkbenchContextFactory implements
			ICompletionContextFactory {

		public HaskellCompletionContext createContext(ITextViewer viewer,
													  int offset)
		{
			return new WorkbenchHaskellCompletionContext(viewer, offset);
		}

	}

	private ICompletionEngine fEngine = null;
	private ICompletionContextFactory fContextFactory;

	public HaskellCAProcessor() {
		this(new CompletionEngine(), new WorkbenchContextFactory());
	}

	public HaskellCAProcessor(ICompletionEngine engine, ICompletionContextFactory factory) {
		fEngine = engine;
		fContextFactory = factory;
	}

	// interface methods of IContentAssistProcessor
	// /////////////////////////////////////////////

	public ICompletionProposal[] computeCompletionProposals(
			final ITextViewer viewer, final int offset)
	{
		HaskellCompletionContext context = fContextFactory.createContext(viewer, offset);
		return getCompletionEngine().computeProposals(context);
	}

	public IContextInformation[] computeContextInformation(
			final ITextViewer viewer, final int documentOffset) {
		// TODO Auto-generated method stub
		return null;
	}

	public char[] getCompletionProposalAutoActivationCharacters() {
		// TODO get from pref and update on pref change
		// return new char[] { '.' };
		return null;
	}

	public char[] getContextInformationAutoActivationCharacters() {
		// TODO Auto-generated method stub
		return null;
	}

	public String getErrorMessage() {
		// return null to indicate we had no problems
		return null;
	}

	public IContextInformationValidator getContextInformationValidator() {
		// TODO Auto-generated method stub
		return null;
	}

	protected ICompletionEngine getCompletionEngine() {
		if (fEngine == null) {
			fEngine = new CompletionEngine();
		}
		return fEngine;
	}

}