package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.types.TokenDef;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.SyntaxPreviewer;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.rules.IPartitionTokenScanner;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;
import org.json.JSONArray;

/**
 * Uses Scion tokenTypesArbitrary function to get tokens for a haskell source
  *
  * @author JP Moresmau
 */
public class ScionTokenScanner implements IPartitionTokenScanner, IEditorPreferenceNames {
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

  private final Map<String,IToken> tokenByTypes;

  public ScionTokenScanner(final ScannerManager man,final ScionInstance instance,final IFile file){
    this.man=man;
    this.instance=instance;
    this.file=file;

    this.tokenByTypes = new HashMap<String, IToken>() {
      // Eclipse insists on a serial version identifier, not that this hash map will ever
      // get serialized...
      private static final long serialVersionUID = 3579246300065591883L;
      {
        // Literal string
        put( "LS", man.createToken( EDITOR_STRING_COLOR, EDITOR_STRING_BOLD ) );
        // Literal character
        put( "LC", man.createToken( EDITOR_CHAR_COLOR, EDITOR_CHAR_BOLD ) );
        put( "D", man.createToken( EDITOR_COMMENT_COLOR,EDITOR_COMMENT_BOLD  ) );
        put( "DL", man.createToken( EDITOR_LITERATE_COMMENT_COLOR, EDITOR_LITERATE_COMMENT_BOLD  ) );
        // Keyword
        put( "K", man.createToken( EDITOR_KEYWORD_COLOR, EDITOR_KEYWORD_BOLD   ) );
        put( "EK", man.createToken( EDITOR_KEYWORD_COLOR, EDITOR_KEYWORD_BOLD   ) );
        put( "LI", man.createToken( EDITOR_NUMBER_COLOR, EDITOR_NUMBER_BOLD   ) );
        put( "LR", man.createToken( EDITOR_NUMBER_COLOR, EDITOR_NUMBER_BOLD   ) );
        put( "LW", man.createToken( EDITOR_NUMBER_COLOR, EDITOR_NUMBER_BOLD   ) );
        put( "LF", man.createToken( EDITOR_NUMBER_COLOR, EDITOR_NUMBER_BOLD   ) );
        put( "LW", man.createToken( EDITOR_NUMBER_COLOR, EDITOR_NUMBER_BOLD   ) );
        put( "IC", man.createToken( EDITOR_CON_COLOR, EDITOR_CON_BOLD  ) );
        put( "IV", man.createToken( EDITOR_VAR_COLOR, EDITOR_VAR_BOLD   ) );
        put( "S", man.createToken( EDITOR_SYMBOL_COLOR, EDITOR_SYMBOL_BOLD   ) );
        put( "SS", man.createToken( EDITOR_SYMBOL_COLOR, EDITOR_SYMBOL_BOLD   ) );
        put( "PP", man.createToken( EDITOR_CPP_COLOR, EDITOR_CPP_BOLD   ) );
        put( "TH", man.createToken( EDITOR_TH_COLOR, EDITOR_TH_BOLD   ) );
      }
    };
  }

  public int getTokenLength() {
    if (currentTokenDef!=null){
      return currentLength;

    }
    return 0;
  }

  public int getTokenOffset() {
    if (currentTokenDef!=null){
      return currentOffset;
    }
    return 0;
  }

  public IToken nextToken() {

     do {
       if (tokenDefs!=null && tokenDefs.hasNext()){
         TokenDef nextTokenDef=tokenDefs.next();
         try {
           int nextOffset=nextTokenDef.getLocation().getStartOffset( doc );
           int nextEnd=nextTokenDef.getLocation().getEndOffset( doc );
           int end=Math.min( offset+length,nextEnd);

           IToken nextToken=getTokenFromTokenDef( nextTokenDef);
           if (currentToken!=null && currentToken.getData().equals( nextToken.getData() ) &&
               currentOffset+currentLength<nextOffset){
             nextOffset= currentOffset+currentLength;
           }
           int nextLength=end-nextOffset;
           currentLength=nextLength;
           currentOffset=nextOffset;
           currentTokenDef=nextTokenDef;
           currentToken=nextToken;

           if (currentOffset>offset+length)  {
             return Token.EOF;
           }


         } catch (BadLocationException ble){
           HaskellUIPlugin.log( ble );
         }
       } else {
         return Token.EOF;
       }
    } while(currentOffset<offset);
    return currentToken;

  }

  public void setRange( final IDocument document, final int offset, final int length ) {
    currentTokenDef=null;
 //   currentToken=null;
    tokenDefs=null;


      if (instance!=null){

        String newContents=document.get();
        if (!document.equals( doc ) || !newContents.equals( contents ) || lTokenDefs==null){
          doc=document;
          contents=newContents;
          lTokenDefs=instance.tokenTypes(file, contents );
        }
      } else {
        try {
          InputStream stream = SyntaxPreviewer.class.getResourceAsStream( "preview.json" );
          // preview file
          JSONArray result=new JSONArray( ResourceUtil.readStream( stream ) );
          lTokenDefs=new ArrayList<TokenDef>(result.length());
          for (int i = 0; i < result.length(); ++i) {
            JSONArray arr=result.getJSONArray(i);
            lTokenDefs.add(new TokenDef(arr));
          }
        } catch( Exception ex ) {
          HaskellUIPlugin.log( "Could not read preview file.", ex ); //$NON-NLS-1$
        }
      }
      this.doc=document;
    if (lTokenDefs!=null && lTokenDefs.size()>0){
      tokenDefs=lTokenDefs.listIterator();
    }

    this.offset=offset;
    this.length=length;
  }

  private IToken getTokenFromTokenDef(final TokenDef td){
    IToken tok=tokenByTypes.get(td.getName());
    if (tok!=null){
      return tok;
    }
    return man.createToken( EDITOR_DEFAULT_COLOR, EDITOR_DEFAULT_BOLD );
  }

  public void setPartialRange( final IDocument document, final int offset, final int length,
      final String contentType, final int partitionOffset ) {
    setRange( document, offset, length );
  }
}
