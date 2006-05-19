package net.sf.eclipsefp.haskell.ui.editor.codeassist;

import net.sf.eclipsefp.haskell.core.codeassist.HaskellCompletionContext;

import org.eclipse.jface.text.ITextViewer;

public interface ICompletionContextFactory {

	HaskellCompletionContext createContext(ITextViewer viewer, int offset);

}
