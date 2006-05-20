package net.sf.eclipsefp.haskell.core.codeassist;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;

import net.sf.eclipsefp.haskell.core.halamo.ICompilationUnit;
import net.sf.eclipsefp.haskell.core.halamo.IDeclaration;
import net.sf.eclipsefp.haskell.core.halamo.IHaskellModel;
import net.sf.eclipsefp.haskell.core.halamo.Scope;

public class HaskellCompletionContext implements IHaskellCompletionContext {

	private IHaskellModel fLanguageModel;
	private ICompilationUnit fCompilationUnit;
	private int fOffset;

	protected HaskellCompletionContext() {
		//placeholder constructor
	}
	
	public HaskellCompletionContext(ICompilationUnit unit,
									IHaskellModel model,
									int offset)
	{
		setCompilationUnit(unit);
		setLanguageModel(model);
		setOffset(offset);
	}

	public IHaskellModel getLanguageModel() {
		return fLanguageModel;
	}

	public ICompilationUnit getCompilationUnit() {
		return fCompilationUnit;
	}

	protected void setLanguageModel(IHaskellModel model) {
		this.fLanguageModel = model;
	}

	protected void setCompilationUnit(ICompilationUnit unit) {
		this.fCompilationUnit = unit;
	}

	protected void setOffset(int fOffset) {
		this.fOffset = fOffset;
	}

	public int getOffset() {
		return fOffset;
	}

	public ICompletionProposal[] computeProposals() {
		String completedToken;
		try {
			completedToken = getQualifier(getCompilationUnit(), getOffset());
			List<String> possibilities = computePossibilities(
												getCompilationUnit(),
												getLanguageModel());
			List<ICompletionProposal> result = filterAndConvert(possibilities, completedToken, getOffset());
			return result.toArray(new ICompletionProposal[result.size()]);
		} catch (Exception ex) {
			//ignore the error and just return an empty result 
		}
		return new ICompletionProposal[0];
	}
	
	List<ICompletionProposal> filterAndConvert(List<String> proposals, String prefix, int offset) {
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

	List<String> computePossibilities(ICompilationUnit unit,
											  IHaskellModel model)
	{
		List<String> result = new ArrayList<String>();
		result.addAll(Arrays.asList(HaskellSyntax.getClasses()));
		result.addAll(Arrays.asList(HaskellSyntax.getKeywords()));
		//TODO move this to the scope calculator
		Scope scope = model.getScopeFor(unit.getModules()[0]);
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

	String getQualifier( final ICompilationUnit unit, 
			                     final int offset )
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
//				no more identifier part, so we use what we have collected
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

	private StringBuffer readSourceTillOffset(final ICompilationUnit unit, final int offset) {
		final String source = unit.getOriginalSourceCode();
		return new StringBuffer(source.substring(0, offset));
	}
	
}
