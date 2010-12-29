// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import net.sf.eclipsefp.haskell.core.codeassist.HaskellLexerTokens;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.util.HaskellText;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;

/**
 * Computes the content assist completion proposals and context information.
 *
 * @author Leif Frenzel (original author)
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public class HaskellContentAssistProcessor implements IContentAssistProcessor {
  /** Default constructor */
	public HaskellContentAssistProcessor() {
	  super();
	}

	// =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
  // interface methods of IContentAssistProcessor
  // =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

	/**
	 * {@inheritDoc}
	 */
	public ICompletionProposal[] computeCompletionProposals(final ITextViewer viewer, final int offset)
	{
	  IFile theFile = HaskellUIPlugin.getFile( viewer );
	  IDocument doc = viewer.getDocument();
	  String prefix = getCompletionPrefix( doc, offset );
	  ScionInstance scion = HaskellUIPlugin.getScionInstance( viewer );

	  if ( scion != null ) {
	    int offsetPrefix = offset - prefix.length();

	    if (offsetPrefix < 0) {
        offsetPrefix = 0;
      }

	    Region point = new Region( offsetPrefix, 0 );
	    String token = scion.tokenPreceding( theFile, doc, point );

	    if (HaskellLexerTokens.isImportToken( token )) {
	      return getModuleNames(scion, prefix, offset);
	    }
	  }

		IHaskellCompletionContext context = new HaskellCompletionContext( theFile, doc.get(), offset );
		HSCodeTemplateAssistProcessor templates = new HSCodeTemplateAssistProcessor();
		ICompletionProposal[] contextProposals = context.computeProposals();
		ICompletionProposal[] templateProposals = templates.computeCompletionProposals( viewer, offset );

		// Merge the results together (templates precede generated proposals):
		int totalSize = contextProposals.length + templateProposals.length;
    int endIndex = 0;
		ICompletionProposal[] result = new ICompletionProposal[ totalSize ];

		if ( templateProposals.length > 0 ) {
		  System.arraycopy( templateProposals, 0, result, endIndex, templateProposals.length );
		  endIndex += templateProposals.length;
		}

		if ( contextProposals.length > 0 ) {
		  System.arraycopy( contextProposals, 0, result, endIndex, contextProposals.length );
		}

		return (totalSize > 0 ? result : null);
	}

	public IContextInformation[] computeContextInformation(final ITextViewer viewer, final int documentOffset) {
		// unused
		return null;
	}

	public char[] getCompletionProposalAutoActivationCharacters() {
	  // unused
		return null;
	}

	public char[] getContextInformationAutoActivationCharacters() {
	  // unused
		return null;
	}

	public String getErrorMessage() {
		// return null to indicate we had no problems
		return null;
	}

	public IContextInformationValidator getContextInformationValidator() {
	  // unused
		return null;
	}

	/**
	 * Generate all of the visible module names matching a prefix.
	 */
	private ICompletionProposal[] getModuleNames(final ScionInstance scion, final String prefix, final int offset) {
    List<String> modules = new ArrayList<String>();

    for (String m : scion.moduleGraph() ) {
      if (prefix.length() == 0 || m.startsWith( prefix )) {
        modules.add(m);
      }
    }

    for (String m : scion.listExposedModules( ) ) {
      if (prefix.length() == 0 || m.startsWith( prefix )) {
        modules.add(m);
      }
    }

    if (modules.size() > 0) {
      Collections.sort( modules, String.CASE_INSENSITIVE_ORDER );

      ICompletionProposal[] result = new ICompletionProposal[modules.size()];
      int i = 0;
      final int prefixLength = prefix.length();

      for (String m : modules) {
        result[i] = new CompletionProposal( m, offset - prefixLength, prefixLength, m.length() );
        ++i;
      }

      return result;
    }

    return null;
	}
	/**
	 * Get the completion prefix by reverse lexing from the offset. The reverse lexing process stops at the beginning of the
	 * line on which the editor point (offset) is located.
	 *
	 * @param document The document from which to extract the completion prefix
	 * @param offset The current editor point in the document.
	 * @return The completion prefix string or an empty string if reverse lexing did not find anything useful.
	 */
	public static final String getCompletionPrefix( final IDocument document, final int offset ) {
    // If we're beyond the document limit (how?), return an empty string
    if( offset > document.getLength() ) {
      return new String();
    }

    try {
      IRegion lineAt = document.getLineInformationOfOffset( offset );
      final int lineBegin = lineAt.getOffset();
      int i = offset - 1;
      char ch = document.getChar( i );

      if (HaskellText.isHaskellIdentifierPart( ch )) {
        // Scan backward until non-identifier character
        for (--i; i >= lineBegin && HaskellText.isHaskellIdentifierPart( document.getChar( i ) ); --i) {
          // NOP
        }

        ++i;
        String retval = document.get( i, offset - i );

        if ( !retval.startsWith( "_" ) ) {
          return retval;
        }
      } else if ( HaskellText.isCommentPart( ch ) ) {
        // Scan backward until a non-comment character:
        for (--i; i >= lineBegin && HaskellText.isCommentPart( document.getChar( i ) ); --i) {
          // NOP
        }

        ++i;
        String retval = document.get( i, offset - i );

        // Ensure that the prefix is really the start of a comment.
        if (retval.startsWith( "{-" ) || retval.startsWith( "--" ) ) {
          return retval;
        }
      } else if ( HaskellText.isSymbol( ch ) ) {
        // Scan backward until a non-comment character:
        for (--i; i >= lineBegin && HaskellText.isSymbol( document.getChar( i ) ); --i) {
          // NOP
        }

        ++i;
        return document.get( i, offset - i );
      } else if (!Character.isWhitespace( ch )){
        // Punt! Grab what we can until we hit whitespace
        for (--i; i >= lineBegin && !Character.isWhitespace( document.getChar(i) ); --i) {
          // NOP
        }

        if (++i < offset) {
          return document.get( i, offset - i);
        }
      }
    } catch( BadLocationException e ) {
      // Dunno how we'd generate this exception, but catch it anyway and fall through
    }

    return new String();
	}
}