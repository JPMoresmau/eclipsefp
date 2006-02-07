// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.editor.codeassist;

import java.util.*;

import net.sf.eclipsefp.haskell.core.codeassist.CompletionEngine;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.contentassist.*;

import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import de.leiffrenzel.fp.haskell.core.parser.ParserManager;
import de.leiffrenzel.fp.haskell.ui.HaskellUIPlugin;

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

    IDocument doc = viewer.getDocument();
    String mask = "";
    List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
    try {
      mask = getQualifier( doc, offset );
      IFile tmp = new HaskellFile(viewer.getDocument().get());
      ICompilationUnit unit = ParserManager.getInstance().getParser().parse(tmp);
      //TODO move this conversion to the completion engine
      String[] proposals = getCompletionEngine().complete(unit, offset);
      result.addAll(toProposalList(proposals, offset, mask.length()));
    } catch( BadLocationException ex ) {
      HaskellUIPlugin.log( "Problem while determining start of proposal.", ex );
    } catch(CoreException ex) {
      HaskellUIPlugin.log( "Problem while parsing for proposal.", ex);
    }
    return toArray(result);
  }

  private List<ICompletionProposal> toProposalList( String[] proposals, int offset, int qlen) {
    List<ICompletionProposal> result = new ArrayList<ICompletionProposal>(proposals.length);
    for(String text : proposals) {
      int textLength = text.length();
      int insertOffset = offset - qlen;
      result.add(new CompletionProposal(text, insertOffset, qlen, textLength));
    }
    return result;
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
  
  // helping methods
  //////////////////
  
  private String getQualifier( final IDocument doc, 
                               final int offset ) throws BadLocationException {
    int index = offset;
    StringBuffer sb = new StringBuffer();
    String result = "";
    
    boolean finished = false;
    while( !finished && index > 0 ) {
      char ch = doc.getChar( --index );
      if( Character.isLetterOrDigit( ch ) ) {
        sb.append( ch );
      } else if( ch == '\"' || ch == '\'' ) {
        // striong or char literals are not taken into account
        finished = true;
      } else {
        // no ore identifier part, so we use what we have collected
        result = sb.reverse().toString();
        finished = true;
      }
    }
    if( index == 0 ) {
      // the special case where we have collected sth. but have reached the
      // end of the document meanwhile
      result = sb.reverse().toString();
    }
    return result;
  }
  
  private ICompletionProposal[] toArray( final List<ICompletionProposal> list ) {
    ICompletionProposal[] result = new ICompletionProposal[ list.size() ];
    list.toArray( result );
    return result;
  }
}