package net.sf.eclipsefp.haskell.ui.editor.codeassist;

import net.sf.eclipsefp.haskell.core.codeassist.IHaskellCompletionContext;

import org.eclipse.jface.text.ITextViewer;

public interface ICompletionContextFactory {

	IHaskellCompletionContext createContext(ITextViewer viewer, int offset);

}
