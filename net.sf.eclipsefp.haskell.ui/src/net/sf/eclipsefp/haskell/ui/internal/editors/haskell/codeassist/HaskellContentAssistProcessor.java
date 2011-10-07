// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// Copyright (c) 2011 by Alejandro Serrano
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Map;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.items.Constructor;
import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.Documented;
import net.sf.eclipsefp.haskell.browser.items.Function;
import net.sf.eclipsefp.haskell.browser.items.Gadt;
import net.sf.eclipsefp.haskell.browser.items.Instance;
import net.sf.eclipsefp.haskell.browser.items.Module;
import net.sf.eclipsefp.haskell.browser.items.TypeClass;
import net.sf.eclipsefp.haskell.browser.items.TypeSynonym;
import net.sf.eclipsefp.haskell.browser.util.HtmlUtil;
import net.sf.eclipsefp.haskell.browser.util.ImageCache;
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports.AnImport;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports.ImportsManager;
import net.sf.eclipsefp.haskell.util.HaskellText;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
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
 * @author Alejandro Serrano
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
    , IMPORT_LIST
    , TYCON_CONTEXT
    , CONID_CONTEXT
  }

  /** Default number of tokens to grab before point when determining completion context */
 // private final static int NUM_PRECEDING_TOKENS = 10;

  /** The current completion context state */
  private CompletionContext context;
  /** The original prefix offset */
  private int prefixOffsetAnchor;

  // Module context variables:
  /** Module names in the modules graph */
  private ArrayList<String> moduleGraphNames;
  /** Module names exposed by the cabal project file */
  private ArrayList<String> exposedModules;

  /** The module name for import list completion */
  private String moduleName;

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
	  //ScionTokenScanner sts=((HaskellEditor)HaskellUIPlugin.getTextEditor( viewer )).getScanner();
	  IFile theFile = HaskellUIPlugin.getFile( viewer );
	  IDocument doc = viewer.getDocument();
	  // Figure out what we're doing...
    //ScionInstance scion = HaskellUIPlugin.getScionInstance( viewer );

    prefix = getCompletionPrefix( doc, offset );
    switch (context) {
      case NO_CONTEXT: {
        //if ( scion != null ) {
          int offsetPrefix = offset - prefix.length();

          try {
            Location lineBegin = new Location(theFile.toString(), doc, doc.getLineInformationOfOffset( offset ));

            if (offsetPrefix < 0) {
              offsetPrefix = 0;
            }

            //Region point = new Region( offsetPrefix, 0 );
            //HaskellLexerToken[] tokens = scion.tokensPrecedingPoint( NUM_PRECEDING_TOKENS, theFile, doc, point );
            IRegion lineR=doc.getLineInformationOfOffset( offsetPrefix );
            String line=doc.get(lineR.getOffset(),lineR.getLength());

            String lineContents = doc.get( lineBegin.getStartOffset( doc ), offset - lineBegin.getStartOffset( doc ) );
            if (lineContents.trim().startsWith( "import" ) && lineContents.contains( "(" )) {
              context = CompletionContext.IMPORT_LIST;
              // Get the module name
              String[] words = lineContents.trim().split( "[ ]+" );
              if (words.length > 1) {
                moduleName = words[1];
                if (moduleName.equals( "qualified" )) {
                  if (words.length > 2) {
                    moduleName = words[2];
                  } else {
                    moduleName = null;
                  }
                }
              } else {
                moduleName = null;
              }
              // Return imports list
              return importsList( viewer, theFile, doc, offset );
            }

              if (line.startsWith( "import" )) {
                return moduleNamesContext(theFile,offset);
              } else if (line.contains("::") || line.contains("->")) {
                return defaultCompletionContext( viewer, theFile, doc, offset, true );
              }

            //if (tokens != null) {
            //  if (LexerTokenCategories.hasImportContext( tokens, lineBegin )) {
            //    return moduleNamesContext(theFile,offset);
            //  } else if (LexerTokenCategories.hasTyConContext( tokens, lineBegin )) {
             //   return defaultCompletionContext( viewer, theFile, doc, offset, true );
           //   }
           // }
          } catch (BadLocationException ble) {
            // Ignore, pass through to default completion context.
          }

          return defaultCompletionContext( viewer, theFile, doc, offset, false );
//        }

//        break;
      }

      case DEFAULT_CONTEXT: {
        return defaultCompletionContext( viewer, theFile, doc, offset, false );
      }

      case IMPORT_STMT: {
        return filterModuleNames( offset );
      }

      case TYCON_CONTEXT: {
        return defaultCompletionContext( viewer, theFile, doc, offset, true );
      }

      case IMPORT_LIST: {
        return importsList( viewer, theFile, doc, offset );
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
		return new char[] { '.' };
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
	  moduleName = null;
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
	  String retval = lexCompletionPrefix( doc, offset );
    prefixOffsetAnchor = offset - retval.length();

    return retval;
}

	/**
	 * Default completion context, if no other context can be determined.
	 */
	private ICompletionProposal[] defaultCompletionContext( final ITextViewer viewer, final IFile theFile, final IDocument doc,
	                                                        final int offset, final boolean typesHavePriority ) {
	  context = typesHavePriority ? CompletionContext.TYCON_CONTEXT : CompletionContext.DEFAULT_CONTEXT;

	  HaskellCompletionContext haskellCompletions = new HaskellCompletionContext( doc.get(), offset );
	  // ICompletionProposal[] haskellProposals = haskellCompletions.computeProposals();
    HSCodeTemplateAssistProcessor templates = new HSCodeTemplateAssistProcessor();
    ICompletionProposal[] templateProposals = templates.computeCompletionProposals( viewer, offset );

    // Get rest of proposals
    String prefix = haskellCompletions.getPointedQualifier();
    ImportsManager mgr = new ImportsManager( theFile, doc );
    Map<String, Documented> decls = mgr.getDeclarations();
    ArrayList<String> elts = new ArrayList<String>();
    ArrayList<String> typeElts = new ArrayList<String>();
    for ( Map.Entry<String, Documented> s : decls.entrySet() ) {
      if ( s.getKey().startsWith( prefix ) ) {
        if (s.getValue() instanceof Constructor || s.getValue() instanceof Function) {
          elts.add( s.getKey() );
        } else if ( s.getValue() instanceof Gadt || s.getValue() instanceof TypeClass ||
            s.getValue() instanceof TypeSynonym ) {
          typeElts.add( s.getKey() );
        }
      }
    }
    Comparator<String> pointedComparator = new Comparator<String>() {

      public int compare( final String a, final String b ) {
        boolean aPointed = a.indexOf( '.' ) != -1;
        boolean bPointed = b.indexOf( '.' ) != -1;
        if (aPointed && !bPointed) {
          return 1;
        } else if (!aPointed && bPointed) {
          return -1;
        } else {
          return a.compareToIgnoreCase( b );
        }
      }

    };
    Collections.sort( elts, pointedComparator );
    Collections.sort( typeElts, pointedComparator );

    // Merge the results together (templates precede generated proposals):
    int totalSize = templateProposals.length + elts.size() + typeElts.size();
    int endIndex = 0;
    ICompletionProposal[] result = new ICompletionProposal[ totalSize ];

    if ( templateProposals.length > 0 ) {
      System.arraycopy( templateProposals, 0, result, endIndex, templateProposals.length );
      endIndex += templateProposals.length;
    }

    final int plength = prefix.length();

    int i = 0;

    if (typesHavePriority) {
      i = 0;
      for ( String s : typeElts ) {
        Documented d = decls.get( s );
        result[endIndex + i] =
            new CompletionProposal( s, offset - plength, plength, s.length(),
                ImageCache.getImageForDeclaration( ((Declaration)d).getType() ), s,
                null, HtmlUtil.generateDocument( d.getCompleteDefinition(), d.getDoc() ) );
        i++;
      }
      endIndex += typeElts.size();
    }

    i = 0;
    for ( String s : elts ) {
      Documented d = decls.get( s );
      result[endIndex + i] =
          new CompletionProposal( s, offset - plength, plength, s.length(),
              d instanceof Constructor ? ImageCache.CONSTRUCTOR : ImageCache.FUNCTION, s,
              null, HtmlUtil.generateDocument( d.getCompleteDefinition(), d.getDoc() ) );
      i++;
    }
    endIndex += elts.size();

    if (!typesHavePriority) {
      i = 0;
      for ( String s : typeElts ) {
        Documented d = decls.get( s );
        result[endIndex + i] =
            new CompletionProposal( s, offset - plength, plength, s.length(),
                ImageCache.getImageForDeclaration( ((Declaration)d).getType() ), s,
                null, HtmlUtil.generateDocument( d.getCompleteDefinition(), d.getDoc() ) );
        i++;
      }
      endIndex += typeElts.size();
    }

    return (totalSize > 0 ? result : null);
	}

	private ICompletionProposal[] importsList( final ITextViewer viewer, final IFile theFile,
	    final IDocument doc, final int offset ) {
	  // Case when we don't know the module name yet
	  if (moduleName == null) {
      return new ICompletionProposal[0];
    }

	  // Get prefix
	  HaskellCompletionContext haskellCompletions = new HaskellCompletionContext( doc.get(), offset   );
	  String prefix = haskellCompletions.getPointedQualifier();
	  int plength = prefix.length();
	  // Reuse the general "imports" code, getting out the qualified names
	  AnImport imp = new AnImport( moduleName, null, true, false, null );
	  Map<String, Documented> decls = imp.getDeclarations( theFile.getProject(), theFile, doc );

	  ArrayList<String> names = new ArrayList<String>();
	  for (Map.Entry<String, Documented> decl : decls.entrySet()) {
	    String s = decl.getKey();
	    if (s.indexOf( '.' ) == -1 && s.startsWith( prefix )) {
        // Don't add qualified imports
        Documented d = decl.getValue();
        if (!(d instanceof Instance) && !(d instanceof TypeClass)) {
          names.add( s );
        }
      }
	  }
	  Collections.sort( names );

	  ICompletionProposal[] r = new ICompletionProposal[names.size()];
	  for (int i = 0; i < names.size(); i++) {
	    String s = names.get( i );
	    Documented d = decls.get( s );
	    r[i] = new CompletionProposal( s, offset - plength, plength, s.length(),
          d instanceof Constructor ? ImageCache.CONSTRUCTOR : ImageCache.getImageForDeclaration( ((Declaration)d).getType() ),
          s, null, HtmlUtil.generateDocument( d.getCompleteDefinition(), d.getDoc() ) );
	  }

	  return r;
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
//	private ICompletionProposal[] moduleNamesContext(final ScionInstance scion, final int offset) {
//	  // Grab all of the module names, keep them cached for the duration of the completion session
//
//
//	  moduleGraphNames = new ArrayList<String>();
//	  moduleGraphNames.addAll( scion.moduleGraph() );
//
//	  exposedModules = new ArrayList<String>();
//	  exposedModules.addAll( scion.listExposedModules() );
//	  context = CompletionContext.IMPORT_STMT;
//
//	  return filterModuleNames( offset );
//	}

	private ICompletionProposal[] moduleNamesContext(final IFile file, final int offset) {
	  moduleGraphNames = new ArrayList<String>();
	  for (PackageDescriptionStanza pds: ResourceUtil.getApplicableStanzas( new IFile[]{file} )){
	    moduleGraphNames.addAll(pds.listAllModules());
	  }
	  exposedModules = new ArrayList<String>();
	  return filterModuleNames( offset );
	}
	/**
	 * Filter module names given a matching prefix.
	 *
	 * @param prefix The module name prefix
	 * @param offset The offset into the document where the completions will be inserted.
	 * @return An ICompletionProposal array of matching completions, or null if none.
	 */
	private ICompletionProposal[] filterModuleNames( final int offset ) {
    // List<String> modules = new ArrayList<String>();
    final String normalizedPrefix = prefix.toLowerCase();

    HashSet<String> modules = new HashSet<String>();

    for (String m : BrowserPlugin.getSharedInstance().getCachedModuleNames()) {
      if (prefix.length() == 0 || m.toLowerCase().startsWith( normalizedPrefix )) {
        modules.add(m);
      }
    }

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
      String[] modulesA = modules.toArray( new String[modules.size()] );
      Arrays.sort( modulesA, String.CASE_INSENSITIVE_ORDER );

      ICompletionProposal[] result = new ICompletionProposal[modulesA.length];
      int i = 0;
      final int prefixLength = prefix.length();

      for (String m : modulesA) {
        Module realM = BrowserPlugin.getSharedInstance().getCachedModule( m );
        result[i] = new CompletionProposal( m, prefixOffsetAnchor, prefixLength, m.length(), ImageCache.MODULE, m, null,
            realM == null ? "" : HtmlUtil.generateDocument( null, realM.getDoc() ));
        ++i;
      }

      return result;
    }

    return null;
	}

	/**
	 * Filter completion pairs
	 */
  /*private ICompletionProposal[] getTypeCompletions( final ScionInstance scion, final IFile file, final IDocument doc,
      final int offset ) {

    Map<String, String> completionPairs = scion.completionsForTypes( file, doc );
    ArrayList<CompletionProposal> proposals = new ArrayList<CompletionProposal>();

    if( completionPairs != null ) {
      final String normalizedPrefix = prefix.toLowerCase();
      TreeSet<String> sortedKeys = new TreeSet<String>(
          String.CASE_INSENSITIVE_ORDER );
      final int prefixLength = prefix.length();

      sortedKeys.addAll( completionPairs.keySet() );
      for( String k: sortedKeys ) {
        // Key may be a qualified name
        String name = k;
        int qualifier = k.lastIndexOf( "." );

        if( qualifier > 0 ) {
          name = k.substring( qualifier + 1 );
        }

        if( prefix.length() == 0
            || name.toLowerCase().startsWith( normalizedPrefix ) ) {
          // String fullProposal = name + " -- " + completionPairs.get( k );
          //
          // if( !name.equals( k ) ) {
          //   fullProposal = fullProposal + " as " + k;
          // }

          CompletionProposal proposal = new CompletionProposal( k,
              prefixOffsetAnchor, prefixLength, k.length(), ImageCache.TYPE, k,
              null, "" );
          proposals.add( proposal );
        }
      }
    }
    return proposals.toArray( new ICompletionProposal[ proposals.size() ] );
  }*/

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

      if (HaskellText.isHaskellIdentifierPart( ch ) || ch == '.') {
        // Scan backward until non-identifier character
        for (--i; i >= lineBegin; --i) {
          char innerC = document.getChar( i );
          if ( !HaskellText.isHaskellIdentifierPart(innerC) && innerC != '.' ) {
            break;
          }
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
      } else if (ch == '(' || ch == ')') {
        // Don't include parentheses in a prefix, e.g., ":: (<point>".
        return new String();
      } else if (!Character.isWhitespace( ch )) {
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
	    // HaskellUIPlugin.log( "CA session starts, prefix = '" + (prefix != null ? prefix : "<null>") + "', context = " + context, null );

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
      // HaskellUIPlugin.log( "CA session restarts, prefix = '" + (prefix != null ? prefix : "<null>") + "', context = " + context, null );
    }

    public void selectionChanged( final ICompletionProposal proposal, final boolean smartToggle ) {
      // HaskellUIPlugin.log( "CA session selection changed, prefix = '" + (prefix != null ? prefix : "<null>") + "', context = " + context, null );
      // NOP
    }

	}
}