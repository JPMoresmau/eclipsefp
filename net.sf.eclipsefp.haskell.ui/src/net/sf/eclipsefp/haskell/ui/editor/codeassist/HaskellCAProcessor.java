// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.editor.codeassist;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import org.eclipse.core.runtime.CoreException;
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

		private static NullCompletionContext instance = null;

		public static NullCompletionContext getInstance() {
			if (null == instance)
				instance = new NullCompletionContext();
			return instance ;
		}
		
		@Override
		public ICompletionProposal[] computeProposals() {
			return new ICompletionProposal[0];
		}
		
	}

	private static class WorkbenchContextFactory implements
			ICompletionContextFactory {

		public IHaskellCompletionContext createContext(final ITextViewer viewer,
													  final int offset)
		{
			try {
				return new WorkbenchHaskellCompletionContext(viewer, offset);
			} catch (CoreException ex) {
				HaskellCorePlugin.log("Error when parsing the viewer "+
						"contents. Aborting code assistance.", ex);
				return NullCompletionContext.getInstance();
			}
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

	public ICompletionProposal[] computeCompletionProposals(
			final ITextViewer viewer, final int offset)
	{
		IHaskellCompletionContext context = fContextFactory.createContext(viewer, offset);
		return context.computeProposals();
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

}