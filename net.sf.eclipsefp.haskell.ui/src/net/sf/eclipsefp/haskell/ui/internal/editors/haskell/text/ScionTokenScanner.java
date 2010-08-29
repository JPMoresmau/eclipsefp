package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
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


public class ScionTokenScanner implements IPartitionTokenScanner, IEditorPreferenceNames {
  private final ScannerManager man;
  private final ScionInstance instance;
  private final IFile file;

  private IDocument doc;

  private TokenDef currentTokenDef;
  private List<TokenDef> lTokenDefs;
  private ListIterator<TokenDef> tokenDefs;
  private IToken currentToken;
  private int currentOffset;
  private int currentLength;

  private int offset;
  private int length;

  public ScionTokenScanner(final ScannerManager man,final ScionInstance instance,final IFile file){
    this.man=man;
    this.instance=instance;
    this.file=file;
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

       if (currentOffset<offset){
         return nextToken();
       }
       if (currentOffset>offset+length)  {
         return Token.EOF;
       }
       return currentToken;
     } catch (BadLocationException ble){
       HaskellUIPlugin.log( ble );
     }
   }
     return Token.EOF;

  }

  public void setRange( final IDocument document, final int offset, final int length ) {
    currentTokenDef=null;
 //   currentToken=null;
    tokenDefs=null;

      this.doc=document;
      if (instance!=null){
        lTokenDefs=instance.tokenTypes(file, document.get() );
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

    if (lTokenDefs!=null && lTokenDefs.size()>0){
      tokenDefs=lTokenDefs.listIterator();
    }

    this.offset=offset;
    this.length=length;
  }

  private IToken getTokenFromTokenDef(final TokenDef td){
    if ("LS".equals(td.getName())){
      return man.createToken( EDITOR_STRING_COLOR, EDITOR_STRING_BOLD );
    } else if ("LC".equals(td.getName())){
      return man.createToken( EDITOR_CHAR_COLOR, EDITOR_CHAR_BOLD );
    } else if ("D".equals(td.getName())){
      return man.createToken( EDITOR_COMMENT_COLOR,
          EDITOR_COMMENT_BOLD );
    } else if ("DL".equals(td.getName())){
      return man.createToken( EDITOR_LITERATE_COMMENT_COLOR,
          EDITOR_LITERATE_COMMENT_BOLD );
    }else if ("K".equals(td.getName()) || "EK".equals(td.getName()) ){
      return man.createToken(EDITOR_KEYWORD_COLOR,
          EDITOR_KEYWORD_BOLD );
    }
    return man.createToken( EDITOR_DEFAULT_COLOR,
        EDITOR_DEFAULT_BOLD );
  }

  public void setPartialRange( final IDocument document, final int offset, final int length,
      final String contentType, final int partitionOffset ) {
    setRange( document, offset, length );

  }
}
