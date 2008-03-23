package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.codeassist.HaskellSyntax;
import net.sf.eclipsefp.haskell.core.halamo.ICompilationUnit;
import net.sf.eclipsefp.haskell.core.halamo.IDeclaration;
import net.sf.eclipsefp.haskell.core.halamo.IHaskellModel;
import net.sf.eclipsefp.haskell.core.halamo.IModule;
import net.sf.eclipsefp.haskell.core.halamo.ITypeSignature;
import net.sf.eclipsefp.haskell.core.halamo.Scope;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;

public class HaskellCompletionContext implements IHaskellCompletionContext {

	private IHaskellModel fLanguageModel;
	private ICompilationUnit fCompilationUnit;
	private int fOffset;

	protected HaskellCompletionContext() {
		//placeholder constructor
	}

	public HaskellCompletionContext(final ICompilationUnit unit,
									final IHaskellModel model,
									final int offset)
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

	protected void setLanguageModel(final IHaskellModel model) {
		this.fLanguageModel = model;
	}

	protected void setCompilationUnit(final ICompilationUnit unit) {
		this.fCompilationUnit = unit;
	}

	protected void setOffset(final int fOffset) {
		this.fOffset = fOffset;
	}

	public int getOffset() {
		return fOffset;
	}

	public ICompletionProposal[] computeProposals() {
		String completedToken;
		try {
			completedToken = getQualifier(getCompilationUnit(), getOffset());

			List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
			searchScope(completedToken, result);
			searchImportableModules(completedToken, result);
			searchPreludeAndKeywords(completedToken, result);
			return result.toArray(new ICompletionProposal[result.size()]);
		} catch (Exception ex) {
			// ignore the error and just return an empty result
		}
		return new ICompletionProposal[0];
	}

	private void searchPreludeAndKeywords(final String prefix,
		final List<ICompletionProposal> result)
	{
		searchStringList(prefix, HaskellSyntax.getClasses(), result);
		searchStringList(prefix, HaskellSyntax.getKeywords(), result);
	}

	private void searchStringList(final String prefix, final String[] names,
		final List<ICompletionProposal> result)
	{
		final int offset = getOffset();
		final int plength = prefix.length();

		for(String name : names) {
			if (name.startsWith(prefix)) {
				result.add(new CompletionProposal(name, offset - plength,
				                                  plength,
				                                  name.length()));

			}
		}
	}

	private void searchImportableModules(final String prefix,
		final List<ICompletionProposal> result)
	{
		final int offset = getOffset();
		final int plength = prefix.length();

		for(IModule m : getLanguageModel().getModules()) {
			final String moduleName = m.getName();
			if (moduleName.startsWith(prefix)) {
				result.add(new CompletionProposal(moduleName, offset - plength,
				                                  plength,
				                                  moduleName.length()));
			}
		}
	}

	private void searchScope(final String prefix, final List<ICompletionProposal> result) {
		if (prefix.length() == 0) {
			return;
		}

		final IModule module = getCompilationUnit().getModules()[0];
		Scope scope = getLanguageModel().getScopeFor(module);

		searchDeclarations(prefix, result, module);

		List<IModule> modules = scope.getAvailableModules();
		for(IModule m : modules) {
			searchDeclarations(prefix, result, m);
		}
	}

	private void searchDeclarations(final String prefix,
		final List<ICompletionProposal> result,
		final IModule module)
	{
		final String moduleName = module.getName();
		final int offset = getOffset();
		final int plength = prefix.length();
		final IDeclaration[] decls = module.getDeclarations();

		for(IDeclaration decl : decls) {
			final String declName = decl.getName();
			if (!(decl instanceof ITypeSignature) &&
				declName.startsWith(prefix))
			{
				final CompletionProposal proposal = new CompletionProposal(
				    declName, offset - plength, plength, declName.length(), null,
				    declName + " - " + moduleName, null, null);
				result.add(proposal);
			}
		}
	}

	private String getQualifier( final ICompilationUnit unit,
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
//				string or char literals are not taken into account
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

	private boolean isIdentifierChar(final char ch) {
		return Character.isLetterOrDigit(ch) || "_'".indexOf(ch) > -1;
	}

	private StringBuffer readSourceTillOffset(final ICompilationUnit unit,
		final int offset)
	{
		final String source = unit.getOriginalSourceCode();
		return new StringBuffer(source.substring(0, offset));
	}

}
