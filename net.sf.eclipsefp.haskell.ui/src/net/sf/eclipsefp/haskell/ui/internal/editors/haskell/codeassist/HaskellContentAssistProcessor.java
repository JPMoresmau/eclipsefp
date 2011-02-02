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
import org.eclipse.jface.text.contentassist.ContentAssistEvent;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.ICompletionListener;
import org.eclipse.jface.text.contentassist.ICompletionListenerExtension;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;

/**
 * Computes the content assist completion proposals and context information. This class is fairly stateful, since
 * what are presented as completion proposals depends on where the editor's point is located and the preceding token's
 * lexical type.
 *
 * @author Leif Frenzel (original author)
 * @author B. Scott Michel (bscottm@ieee.org)
 */
public class HaskellContentAssistProcessor implements IContentAssistProcessor {
  /** The associated content assistant, used to add/remove listeners */
  private final ContentAssistant assistant;
  /** Current completion prefix */
  private String prefix;

  /** The different context states that the completion processor needs to track */
  enum CompletionContext {
      NO_CONTEXT
    , DEFAULT_CONTEXT
    , IMPORT_STMT
    , CONID_CONTEXT
  }

  /** The current completion context state */
  private CompletionContext context;
  /** The original prefix offset */
  private int prefixOffsetAnchor;

  // Module context variables:
  /** Module names in the modules graph */
  private ArrayList<String> moduleGraphNames;
  /** Module names exposed by the cabal project file */
  private ArrayList<String> exposedModules;

  /**
   * The constructor.
   *
   * @param assistant The associated content assistant
   */
	public HaskellContentAssistProcessor(final ContentAssistant assistant) {
	  super();
	  this.assistant = assistant;
	  internalReset();

	  // Add the listener, who modulates the completion context
	  this.assistant.addCompletionListener( new CAListener() );
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

    prefix = getCompletionPrefix( doc, offset );
    switch (context) {
      case NO_CONTEXT: {
        // Figure out what we're doing...
        ScionInstance scion = HaskellUIPlugin.getScionInstance( viewer );
        if ( scion != null ) {
          int offsetPrefix = offset - prefix.length();

          if (offsetPrefix < 0) {
            offsetPrefix = 0;
          }

          Region point = new Region( offsetPrefix, 0 );
          String token = scion.tokenPrecedingPoint( theFile, doc, point );

          if (HaskellLexerTokens.isImportContext( token )) {
            return moduleNamesContext(scion, offset);
          } else if (HaskellLexerTokens.isTyConContext( token )) {
            return typeConstructorContext(scion, theFile, doc, offset);
          } else {
            return defaultCompletionContext(viewer, theFile, doc, offset);
          }
        }

        break;
      }

      case DEFAULT_CONTEXT: {
        return defaultCompletionContext( viewer, theFile, doc, offset );
      }

      case IMPORT_STMT: {
        return filterModuleNames( offset );
      }

      case CONID_CONTEXT: {
        return null;
      }
    }

    return null;
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

	/** Hard reset internal state */
	private void internalReset() {
	  context = CompletionContext.NO_CONTEXT;
    prefixOffsetAnchor = -1;
	  prefix = new String();
	  moduleGraphNames = null;
	  exposedModules = null;
	}

	/** Get the completion prefix from the prefix offset, if non-zero, or reverse lex */
	private String getCompletionPrefix( final IDocument doc, final int offset ) {
	  if (prefixOffsetAnchor > 0) {
	    try {
        return doc.get( prefixOffsetAnchor, offset - prefixOffsetAnchor );
      } catch( BadLocationException ex ) {
        // Should not happen, but fall through to lexCompletionPrefix() call
      }
	  }

	  // Prefix offset anchor isn't set, or fell through as the result of the exception
	  prefixOffsetAnchor = offset;
	  return lexCompletionPrefix( doc, offset );
	}

	/**
	 * Default completion context, if no other context can be determined.
	 */
	private ICompletionProposal[] defaultCompletionContext( final ITextViewer viewer, final IFile theFile, final IDocument doc,
	                                                        final int offset ) {
	  context = CompletionContext.DEFAULT_CONTEXT;

    IHaskellCompletionContext haskellCompletions = new HaskellCompletionContext( theFile, doc.get(), offset );
    HSCodeTemplateAssistProcessor templates = new HSCodeTemplateAssistProcessor();
    ICompletionProposal[] haskellProposals = haskellCompletions.computeProposals();
    ICompletionProposal[] templateProposals = templates.computeCompletionProposals( viewer, offset );

    // Merge the results together (templates precede generated proposals):
    int totalSize = haskellProposals.length + templateProposals.length;
    int endIndex = 0;
    ICompletionProposal[] result = new ICompletionProposal[ totalSize ];

    if ( templateProposals.length > 0 ) {
      System.arraycopy( templateProposals, 0, result, endIndex, templateProposals.length );
      endIndex += templateProposals.length;
    }

    if ( haskellProposals.length > 0 ) {
      System.arraycopy( haskellProposals, 0, result, endIndex, haskellProposals.length );
    }

    return (totalSize > 0 ? result : null);
	}

	/**
	 * Initialize the state necessary for the 'import' statement's context.
	 *
	 * @param scion The scion-server instance for the document's file/project
	 * @param prefix The prefix used to filter matching module names
	 * @param offset Offset into the document where proposals could be inserted
	 *
	 * @return A ICompletionProposal array of matching module names, or null, if none.
	 */
	private ICompletionProposal[] moduleNamesContext(final ScionInstance scion, final int offset) {
	  // Grab all of the module names, keep them cached for the duration of the completion session

	  moduleGraphNames = new ArrayList<String>();
	  moduleGraphNames.addAll( scion.moduleGraph() );

	  exposedModules = new ArrayList<String>();
	  exposedModules.addAll( scion.listExposedModules() );
	  context = CompletionContext.IMPORT_STMT;

	  return filterModuleNames( offset);
	}

	/**
	 * Filter module names given a matching prefix.
	 *
	 * @param prefix The module name prefix
	 * @param offset The offset into the document where the completions will be inserted.
	 * @return An ICompletionProposal array of matching completions, or null if none.
	 */
	private ICompletionProposal[] filterModuleNames( final int offset ) {
    List<String> modules = new ArrayList<String>();
    final String normalizedPrefix = prefix.toLowerCase();

    for (String m : moduleGraphNames ) {
      if (prefix.length() == 0 || m.toLowerCase().startsWith( normalizedPrefix )) {
        modules.add(m);
      }
    }

    for (String m : exposedModules ) {
      if (prefix.length() == 0 || m.toLowerCase().startsWith( normalizedPrefix )) {
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
	 * Initialize the type constructor context: this will only present type constructors as completions.
	 *
	 * @param scion The scion-server instance that generates the completions
	 * @param offset The current editor point
	 *
	 * @return A ICompletionProposal list or null, if no completions exist
	 */
	private ICompletionProposal[] typeConstructorContext(final ScionInstance scion, final IFile file, final IDocument doc,
	                                                     final int offset) {
	  scion.completionsForTypeConstructors( file, doc );
	  return null;
	}

	/**
	 * Get the completion prefix by reverse lexing from the offset. The reverse lexing process stops at the beginning of the
	 * line on which the editor point (offset) is located, unless a token has been otherwise collected.
	 *
	 * @param document The document from which to extract the completion prefix
	 * @param offset The current editor point in the document.
	 * @return The completion prefix string or an empty string if reverse lexing did not find anything useful.
	 */
	public static final String lexCompletionPrefix( final IDocument document, final int offset ) {
    // If we're beyond the document limit (how?), return an empty string
    if( offset > document.getLength() ) {
      return new String();
    }

    try {
      IRegion lineAt = document.getLineInformationOfOffset( offset );
      final int lineBegin = lineAt.getOffset();
      int i = offset - 1;
      final char ch = document.getChar( i );

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

	/** Content assistant listener: This initializes and manages the transitions between completion context states. */
	private class CAListener implements ICompletionListener, ICompletionListenerExtension {
	  public CAListener() {
	    // NOP
	  }

	  public void assistSessionStarted( final ContentAssistEvent event ) {
	    // HaskellUIPlugin.log( "CA session starts, prefix = '" + (prefix != null ? prefix : "<null") + "', context = " + context, null );

	    // Reset the context to force computeCompletionProposals to figure out what the context,
	    // clean out existing state:
	    HaskellContentAssistProcessor.this.internalReset();
    }

    public void assistSessionEnded( final ContentAssistEvent event ) {
      // HaskellUIPlugin.log( "CA session ends.", null);

      // Reset internal state for completeness
      HaskellContentAssistProcessor.this.internalReset();
    }

    public void assistSessionRestarted( final ContentAssistEvent event ) {
      // HaskellUIPlugin.log( "CA session restarts, prefix = '" + (prefix != null ? prefix : "<null") + "', context = " + context, null );
    }

    public void selectionChanged( final ICompletionProposal proposal, final boolean smartToggle ) {
      // NOP
    }

	}
}