// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.editor.codeassist;

import org.eclipse.core.runtime.CoreException;
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

	public static class NullCompletionContext extends HaskellCompletionContext {
		//TODO write the computeProposals method that doesn't return anything here 
	}

	private static class WorkbenchContextFactory implements
			ICompletionContextFactory {

		public HaskellCompletionContext createContext(ITextViewer viewer,
													  int offset)
		{
			try {
				return new WorkbenchHaskellCompletionContext(viewer, offset);
			} catch (CoreException ex) {
				//TODO this means there was an error when parsing the contents
				//of the viewer and the code assistance cannot go on
				//TODO log the error
				return new NullCompletionContext();
			}
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