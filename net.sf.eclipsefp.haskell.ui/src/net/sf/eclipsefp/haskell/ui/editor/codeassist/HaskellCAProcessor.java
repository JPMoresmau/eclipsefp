// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.editor.codeassist;

import net.sf.eclipsefp.haskell.core.codeassist.CompletionEngine;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.contentassist.*;

import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import de.leiffrenzel.fp.haskell.core.parser.ParserManager;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;

/** <p>computes the code assist completion proposals and context 
  * information.</p>
  * 
  * @author Leif Frenzel
  */
public class HaskellCAProcessor implements IContentAssistProcessor {

  private CompletionEngine fEngine = null;
  
  public HaskellCAProcessor() {
    //placeholder constructor
  }

  public HaskellCAProcessor(CompletionEngine engine) {
    fEngine = engine;
  }

  // interface methods of IContentAssistProcessor
  ///////////////////////////////////////////////
  
  public ICompletionProposal[] computeCompletionProposals(  
                                  final ITextViewer viewer, final int offset ) {

    try {
      IFile tmp = new HaskellFile(viewer.getDocument().get());
      ICompilationUnit unit = ParserManager.getInstance().getParser().parse(tmp);
      //TODO move this conversion to the completion engine
      return getCompletionEngine().complete(unit, offset);
    } catch(CoreException ex) {
      HaskellUIPlugin.log( "Problem while parsing for proposal.", ex);
    }
    return new ICompletionProposal[0];
  }

  public IContextInformation[] computeContextInformation(
                          final ITextViewer viewer, final int documentOffset ) {
    // TODO Auto-generated method stub
    return null;
  }

  public char[] getCompletionProposalAutoActivationCharacters() {
    // TODO get from pref and update on pref change
//    return new char[] { '.' };
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
  
  
  protected CompletionEngine getCompletionEngine() {
    if (fEngine == null) {
      fEngine = new CompletionEngine();
    }
    return fEngine;
  }
  
}