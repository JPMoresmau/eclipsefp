package net.sf.eclipsefp.haskell.core.codeassist;

import net.sf.eclipsefp.haskell.core.halamo.ICompilationUnit;
import net.sf.eclipsefp.haskell.core.halamo.IHaskellModel;

public class HaskellCompletionContext {

	//TODO move the completion logic to this class. it is looking too much like
	//a dumb record
	
	private IHaskellModel fLanguageModel;
	private ICompilationUnit fCompilationUnit;
	private int fOffset;

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
}
