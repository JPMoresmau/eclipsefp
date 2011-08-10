package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import net.sf.eclipsefp.haskell.core.codeassist.IScionTokens;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.types.Location;
import net.sf.eclipsefp.haskell.scion.types.TokenDef;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.rules.IPartitionTokenScanner;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;

/**
 * Uses Scion tokenTypesArbitrary function to get tokens for a haskell source
 *
 * @author JP Moresmau
 */
public class PartitionedScionTokenScanner implements IPartitionTokenScanner,
    IEditorPreferenceNames {

  private final ScannerManager man;
  private final ScionInstance instance;
  private final IFile file;

  private IDocument doc;
  private String contents;

  private TokenDef currentTokenDef;
  private List<TokenDef> lTokenDefs;
  private ListIterator<TokenDef> tokenDefs;
  private IToken currentToken;
  private int currentOffset;
  private int currentLength;

  private int offset;
  private int length;

  private final Map<String, IToken> tokenByTypes;

  private final String[] initialElements;
  private final String[] endElements;
  private final String[] initialComments;
  private final String[] endComments;

  public PartitionedScionTokenScanner( final ScannerManager man,
      final ScionInstance instance, final IFile file ) {
    this( man, instance, file, new String[] { "{" }, new String[] { "}" },
        new String[ 0 ], new String[ 0 ] );
  }

  public PartitionedScionTokenScanner( final ScannerManager man,
      final ScionInstance instance, final IFile file,
      final String[] initialElements, final String[] endElements,
      final String[] initialComments, final String[] endComments ) {
    this.man = man;
    this.instance = instance;
    this.file = file;

    this.initialElements = initialElements;
    this.endElements = endElements;
    this.initialComments = initialComments;
    this.endComments = endComments;

    this.tokenByTypes = new HashMap<String, IToken>() {

      // Eclipse insists on a serial version identifier, not that this hash map
      // will ever
      // get serialized...
      private static final long serialVersionUID = 3579246300065591883L;
      {
        put( IScionTokens.LITERAL_STRING,
            man.createToken( EDITOR_STRING_COLOR, EDITOR_STRING_BOLD ) );
        put( IScionTokens.LITERAL_CHAR,
            man.createToken( EDITOR_CHAR_COLOR, EDITOR_CHAR_BOLD ) );
        put( IScionTokens.DOCUMENTATION_ANNOTATION,
            man.createToken( EDITOR_COMMENT_COLOR, EDITOR_COMMENT_BOLD ) );
        put( IScionTokens.LITERATE_COMMENT, man.createToken(
            EDITOR_LITERATE_COMMENT_COLOR, EDITOR_LITERATE_COMMENT_BOLD ) );
        put( IScionTokens.KEYWORD,
            man.createToken( EDITOR_KEYWORD_COLOR, EDITOR_KEYWORD_BOLD ) );
        put( IScionTokens.GHC_EXTENSION_KEYWORD,
            man.createToken( EDITOR_KEYWORD_COLOR, EDITOR_KEYWORD_BOLD ) );
        put( IScionTokens.LITERAL_INTEGER,
            man.createToken( EDITOR_NUMBER_COLOR, EDITOR_NUMBER_BOLD ) );
        put( IScionTokens.LITERAL_RATIONAL,
            man.createToken( EDITOR_NUMBER_COLOR, EDITOR_NUMBER_BOLD ) );
        put( IScionTokens.LITERAL_WORD,
            man.createToken( EDITOR_NUMBER_COLOR, EDITOR_NUMBER_BOLD ) );
        put( IScionTokens.LITERAL_FLOAT,
            man.createToken( EDITOR_NUMBER_COLOR, EDITOR_NUMBER_BOLD ) );
        put( IScionTokens.IDENTIFIER_CONSTRUCTOR,
            man.createToken( EDITOR_CON_COLOR, EDITOR_CON_BOLD ) );
        put( IScionTokens.IDENTIFIER_VARIABLE,
            man.createToken( EDITOR_VAR_COLOR, EDITOR_VAR_BOLD ) );
        put( IScionTokens.SYMBOL_RESERVED,
            man.createToken( EDITOR_SYMBOL_COLOR, EDITOR_SYMBOL_BOLD ) );
        put( IScionTokens.SYMBOL_SPECIAL,
            man.createToken( EDITOR_SYMBOL_COLOR, EDITOR_SYMBOL_BOLD ) );
        put( IScionTokens.PREPROCESSOR_TEXT,
            man.createToken( EDITOR_CPP_COLOR, EDITOR_CPP_BOLD ) );
        put( IScionTokens.TEMPLATE_HASKELL,
            man.createToken( EDITOR_TH_COLOR, EDITOR_TH_BOLD ) );
      }
    };
  }

  public int getTokenLength() {
    if( currentTokenDef != null ) {
      return currentLength;

    }
    return 0;
  }

  public int getTokenOffset() {
    if( currentTokenDef != null ) {
      return currentOffset;
    }
    return 0;
  }

  public IToken nextToken() {

    do {
      if( tokenDefs != null && tokenDefs.hasNext() ) {
        TokenDef nextTokenDef = tokenDefs.next();
        try {
          int nextOffset = nextTokenDef.getLocation().getStartOffset( doc );
          int nextEnd = nextTokenDef.getLocation().getEndOffset( doc );
          int end = Math.min( offset + length, nextEnd );

          IToken nextToken = getTokenFromTokenDef( nextTokenDef );
          if( currentToken != null
              && currentToken.getData().equals( nextToken.getData() )
              && currentOffset + currentLength < nextOffset ) {
            nextOffset = currentOffset + currentLength;
          }
          int nextLength = end - nextOffset;
          currentLength = nextLength;
          currentOffset = nextOffset;
          currentTokenDef = nextTokenDef;
          currentToken = nextToken;

          if( currentOffset > offset + length ) {
            return Token.EOF;
          }


        } catch( BadLocationException ble ) {
          HaskellUIPlugin.log( ble );
        }
      } else {
        return Token.EOF;
      }
    } while( currentOffset < offset );
    return currentToken;

  }

  public void setRange( final IDocument document, final int offset,
      final int length ) {
    currentTokenDef = null;
    // currentToken=null;
    tokenDefs = null;

    int realOffset = offset, realLength = length;

    try {
      String prevContents = document.get( realOffset, realLength );

      // Check whether this is a comment
      boolean isInitialComment = false, isEndComment = false;
      for( String initialC: initialComments ) {
        if( prevContents.startsWith( initialC ) ) {
          isInitialComment = true;
          break;
        }
      }
      for( String endC: endComments ) {
        if( prevContents.endsWith( endC ) ) {
          isEndComment = true;
          break;
        }
      }
      if( isInitialComment && isEndComment ) {
        // We found a comment
        doc = document;
        contents = prevContents;

        TokenDef def = new TokenDef( IScionTokens.LITERATE_COMMENT,
            new Location( "", document, new Region( realOffset, realLength ) ) );
        lTokenDefs = new ArrayList<TokenDef>();
        lTokenDefs.add( def );
      } else {
        // We are not in a comment
        for( String initialE: initialElements ) {
          if( prevContents.startsWith( initialE ) ) {
            int initialLength = initialE.length();
            realOffset += initialLength;
            realLength -= initialLength;
            for( int i = initialLength; i < prevContents.length(); i++ ) {
              char c = prevContents.charAt( i );
              if( Character.isWhitespace( c ) ) {
                realOffset++;
                realLength--;
              } else {
                break;
              }
            }
            break;
          }
        }
        for( String endE: endElements ) {
          if( prevContents.endsWith( endE ) ) {
            realLength -= endE.length();
            break;
          }
        }

        String newContents = document.get( realOffset, realLength );

        if( !document.equals( doc ) || !newContents.equals( contents )
            || lTokenDefs == null ) {
          doc = document;
          contents = newContents;
          lTokenDefs = instance.tokenTypes( file, contents );
        }

        try {
          // Move the offset to the correct place
          // First get line and character
          int line = document.getLineOfOffset( realOffset );
          int lineOffset = document.getLineOffset( line );
          int column = realOffset - lineOffset;
          for( TokenDef def: lTokenDefs ) {
            def.move( line, column );
          }
        } catch( BadLocationException ble ) {
          HaskellUIPlugin.log( ble );
        }
      }
    } catch( BadLocationException ble ) {
      HaskellUIPlugin.log( ble );
    }

    this.doc = document;
    if( lTokenDefs != null && lTokenDefs.size() > 0 ) {
      tokenDefs = lTokenDefs.listIterator();
    }

    this.offset = realOffset;
    this.length = realLength;
    this.currentTokenDef = null;
    this.currentOffset = this.offset;
    this.currentLength = this.length;
  }

  private IToken getTokenFromTokenDef( final TokenDef td ) {
    IToken tok = tokenByTypes.get( td.getName() );
    if( tok != null ) {
      return tok;
    }
    return man.createToken( EDITOR_DEFAULT_COLOR, EDITOR_DEFAULT_BOLD );
  }

  public void setPartialRange( final IDocument document, final int offset,
      final int length, final String contentType, final int partitionOffset ) {
    setRange( document, offset, length );
  }
}
