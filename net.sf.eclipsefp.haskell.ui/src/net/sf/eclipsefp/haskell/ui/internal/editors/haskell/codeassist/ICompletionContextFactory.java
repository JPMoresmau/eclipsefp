package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;


import org.eclipse.jface.text.ITextViewer;

public interface ICompletionContextFactory {

	IHaskellCompletionContext createContext(ITextViewer viewer, int offset);

}
