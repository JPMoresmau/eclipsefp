package net.sf.eclipsefp.haskell.core.codeassist;

import java.io.*;
import java.util.*;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;

import net.sf.eclipsefp.haskell.core.halamo.*;

public class CompletionEngine implements ICompletionEngine {

	private static interface IScopeStrategy {
		
		public Scope getScopeFor(ICompilationUnit unit);
		
	}
	
	private static class WorkspaceScopeStrategy implements IScopeStrategy {
		public Scope getScopeFor(ICompilationUnit unit) {
			IProject currentProject = unit.getUnderlyingResource().getProject();
			if (null == currentProject)
				return Scope.EMPTY;
			
			
			HaskellModelManager models = HaskellModelManager.getInstance();
			IHaskellModel model = models.getModelFor(currentProject);
			
			if (null == model)
				return Scope.EMPTY;
			
			return model.getScopeFor(unit.getModules()[0]);
		}
	}
	
	private static class ModelScopeStrategy implements IScopeStrategy {
		
		private HaskellLanguageModel fLanguageModel;
		
		public ModelScopeStrategy(HaskellLanguageModel model) {
			fLanguageModel = model;
		}
		
		public Scope getScopeFor(ICompilationUnit unit) {
			return fLanguageModel.getScopeFor(unit.getModules()[0]);
		}
	}

	private IScopeStrategy fScopeCalculator;
	
	public CompletionEngine() {
		fScopeCalculator = new WorkspaceScopeStrategy();
	}
	
	public CompletionEngine(HaskellLanguageModel langModel) {
		fScopeCalculator = new ModelScopeStrategy(langModel);
	}

	public ICompletionProposal[] complete(ICompilationUnit unit, int offset) {
		String completedToken;
		try {
			completedToken = getQualifier(unit, offset);
			List<String> possibilities = computePossibilities(unit);
			List<ICompletionProposal> result = filterAndConvert(possibilities, completedToken, offset);
			return result.toArray(new ICompletionProposal[result.size()]);
		} catch (Exception ex) {
			//ignore the error and just return an empty result 
		}
		return new ICompletionProposal[0];
	}
	
	private List<ICompletionProposal> filterAndConvert(List<String> proposals, String prefix, int offset) {
		List<ICompletionProposal> result = new ArrayList<ICompletionProposal>(proposals.size());
		int qlen = prefix.length();
		if (qlen == 0) {
			return result; 
		}

		for (String prop : proposals) {
			if (prop.startsWith(prefix)) {
				result.add(new CompletionProposal(prop, offset - qlen,
						                          qlen, prop.length()));
			}
		}
		return result;
	}

	private List<String> computePossibilities(ICompilationUnit unit) {
		List<String> result = new ArrayList<String>();
		result.addAll(Arrays.asList(HaskellSyntax.getClasses()));
		result.addAll(Arrays.asList(HaskellSyntax.getKeywords()));
		//TODO move this to the scope calculator
		Scope scope = fScopeCalculator.getScopeFor(unit);
		List<IDeclaration> internalDecls = Arrays.asList(unit.getModules()[0].getDeclarations());
		List<IDeclaration> externalDecls = scope.getAvailableDeclarations();
		List<IDeclaration> decls = new ArrayList<IDeclaration>(internalDecls.size() + externalDecls.size());
		decls.addAll(internalDecls);
		decls.addAll(externalDecls);
		for (IDeclaration decl : decls) {
			result.add(decl.getName());
		}
		
		return result;
	}

	private String getQualifier( final ICompilationUnit unit, 
			                     final int offset ) throws CoreException, IOException
	{
		StringBuffer contents = readSourceTillOffset(unit, offset);

		int index = offset;
		StringBuffer sb = new StringBuffer();
		String result = "";
		
		boolean finished = false;
		while( !finished && index > 0 ) {
			char ch = contents.charAt(--index);
			if( isIdentifierChar(ch) ) {
				sb.append( ch );
			} else if( ch == '\"' || ch == '\'' ) {
//				striong or char literals are not taken into account
				finished = true;
			} else {
//				no ore identifier part, so we use what we have collected
				result = sb.reverse().toString();
				finished = true;
			}
		}
		if( index == 0 ) {
//			the special case where we have collected sth. but have reached the
//			end of the document meanwhile
			result = sb.reverse().toString();
		}
		return result;
	}

	private boolean isIdentifierChar(char ch) {
		return Character.isLetterOrDigit(ch) || "_'".indexOf(ch) > -1;
	}

	private StringBuffer readSourceTillOffset(final ICompilationUnit unit, final int offset) throws CoreException, IOException {
		InputStream in = unit.getUnderlyingResource().getContents();
		StringBuffer contents = new StringBuffer(offset);
		for(int i = 0; i < offset; ++i) {
			contents.append((char) in.read());
		}
		return contents;
	}

	public ICompletionProposal[] computeProposals(HaskellCompletionContext context) {
		return null;
	}

}
