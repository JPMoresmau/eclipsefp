/**
 *  Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.buildwrapper.types.Occurrence;
import net.sf.eclipsefp.haskell.buildwrapper.types.TokenDef;
import net.sf.eclipsefp.haskell.core.codeassist.ITokenTypes;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.rules.IPartitionTokenScanner;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.texteditor.MarkerUtilities;

/**
 * Uses BuildWrapper getTokenTypes function to get tokens for a Haskell source
  *
  * @author JP Moresmau
 */
public class ScionTokenScanner implements IPartitionTokenScanner, IEditorPreferenceNames, org.eclipse.jface.util.IPropertyChangeListener {
  private final ScannerManager man;
  private final IFile file;

  private IDocument doc;
  private String contents;

  private TokenDef currentTokenDef;
  private List<TokenDef> lTokenDefs;
  private List<TokenDef> lMergedTokenDefs;
  private ListIterator<TokenDef> tokenDefs;

  private final Map<List<String>,List<TokenDef>> occurrences=new HashMap<>();
  private final List<List<String>> tokenLocations=new ArrayList<>();

  private IToken currentToken;
  private int currentOffset;
  private int currentLength;

  private int offset;
  private int length;

  private File tgt;

  //private boolean checkedTabs=false;

  protected Map<String,ContentTypeToken> tokenByTypes;

  private Set<TaskTag> tags;
  private boolean caseS;


  /**
   * since we may run in a job for big files, we need a handle to the GUI display
   */
  //private final Display display;

  public ScionTokenScanner(final ScannerManager man,final IFile file){
    this.man=man;
    this.file=file;
    //,final Display display
    //this.display=display;
    getCaseS();

    man.getPreferenceStore().addPropertyChangeListener(this);
    buildTokenTypes();


  }

  private void buildTokenTypes(){
    this.tokenByTypes = new HashMap<String, ContentTypeToken>() {
      // Eclipse insists on a serial version identifier, not that this hash map will ever
      // get serialized...
      private static final long serialVersionUID = 3579246300065591883L;
      {
        put( ITokenTypes.LITERAL_STRING, new ContentTypeToken( HaskellEditor.TEXT_CONTENTTYPE,man.createToken( EDITOR_STRING_COLOR, EDITOR_STRING_BOLD ) ));
        put( ITokenTypes.LITERAL_CHAR, new ContentTypeToken(IDocument.DEFAULT_CONTENT_TYPE,man.createToken( EDITOR_CHAR_COLOR, EDITOR_CHAR_BOLD ) ));
        put( ITokenTypes.DOCUMENTATION_ANNOTATION, new ContentTypeToken( HaskellEditor.TEXT_CONTENTTYPE,man.createToken( EDITOR_DOC_COLOR,EDITOR_DOC_BOLD  ) ));
        put( ITokenTypes.COMMENT, new ContentTypeToken( HaskellEditor.TEXT_CONTENTTYPE,man.createToken( EDITOR_COMMENT_COLOR,EDITOR_COMMENT_BOLD  ) ));
        put( ITokenTypes.PRAGMA, new ContentTypeToken(IDocument.DEFAULT_CONTENT_TYPE,man.createToken( EDITOR_PRAGMA_COLOR,EDITOR_PRAGMA_BOLD  ) ));
        put( ITokenTypes.LITERATE_COMMENT, new ContentTypeToken( HaskellEditor.TEXT_CONTENTTYPE,man.createToken( EDITOR_LITERATE_COMMENT_COLOR, EDITOR_LITERATE_COMMENT_BOLD  ) ));
        put( ITokenTypes.KEYWORD, new ContentTypeToken(IDocument.DEFAULT_CONTENT_TYPE,man.createToken( EDITOR_KEYWORD_COLOR, EDITOR_KEYWORD_BOLD   ) ));
        put( ITokenTypes.GHC_EXTENSION_KEYWORD, new ContentTypeToken(IDocument.DEFAULT_CONTENT_TYPE,man.createToken( EDITOR_KEYWORD_COLOR, EDITOR_KEYWORD_BOLD   ) ));
        put( ITokenTypes.LITERAL_INTEGER, new ContentTypeToken(IDocument.DEFAULT_CONTENT_TYPE,man.createToken( EDITOR_NUMBER_COLOR, EDITOR_NUMBER_BOLD   ) ));
        put( ITokenTypes.LITERAL_RATIONAL, new ContentTypeToken(IDocument.DEFAULT_CONTENT_TYPE,man.createToken( EDITOR_NUMBER_COLOR, EDITOR_NUMBER_BOLD   ) ));
        put( ITokenTypes.LITERAL_WORD, new ContentTypeToken(IDocument.DEFAULT_CONTENT_TYPE,man.createToken( EDITOR_NUMBER_COLOR, EDITOR_NUMBER_BOLD   ) ));
        put( ITokenTypes.LITERAL_FLOAT, new ContentTypeToken(IDocument.DEFAULT_CONTENT_TYPE,man.createToken( EDITOR_NUMBER_COLOR, EDITOR_NUMBER_BOLD   ) ));
        put( ITokenTypes.IDENTIFIER_CONSTRUCTOR, new ContentTypeToken(IDocument.DEFAULT_CONTENT_TYPE,man.createToken( EDITOR_CON_COLOR, EDITOR_CON_BOLD  ) ));
        put( ITokenTypes.IDENTIFIER_VARIABLE, new ContentTypeToken(IDocument.DEFAULT_CONTENT_TYPE,man.createToken( EDITOR_VAR_COLOR, EDITOR_VAR_BOLD   ) ));
        put( ITokenTypes.SYMBOL_VARIABLE, new ContentTypeToken(IDocument.DEFAULT_CONTENT_TYPE,man.createToken( EDITOR_VARSYM_COLOR, EDITOR_VARSYM_BOLD   ) ));
        put( ITokenTypes.SYMBOL_RESERVED, new ContentTypeToken(IDocument.DEFAULT_CONTENT_TYPE,man.createToken( EDITOR_SYMBOL_COLOR, EDITOR_SYMBOL_BOLD   ) ));
        put( ITokenTypes.SYMBOL_SPECIAL, new ContentTypeToken(IDocument.DEFAULT_CONTENT_TYPE,man.createToken( EDITOR_SYMBOL_COLOR, EDITOR_SYMBOL_BOLD   ) ));
        put( ITokenTypes.PREPROCESSOR_TEXT, new ContentTypeToken(IDocument.DEFAULT_CONTENT_TYPE,man.createToken( EDITOR_CPP_COLOR, EDITOR_CPP_BOLD   ) ));
        put( ITokenTypes.TEMPLATE_HASKELL, new ContentTypeToken(IDocument.DEFAULT_CONTENT_TYPE,man.createToken( EDITOR_TH_COLOR, EDITOR_TH_BOLD   ) ));
      }
    };
  }

  @Override
  public int getTokenLength() {
    if (currentTokenDef!=null){
      return currentLength;

    }
    return 0;
  }

  @Override
  public int getTokenOffset() {
    if (currentTokenDef!=null){
      return currentOffset;
    }
    return 0;
  }

  @Override
  public IToken nextToken() {
     do {
       if (tokenDefs!=null && tokenDefs.hasNext()){
         TokenDef nextTokenDef=tokenDefs.next();
         try {
           int nextOffset=nextTokenDef.getLocation().getStartOffset( doc );
           //nextOffset=Math.max( nextOffset, offset );
           int nextEnd=nextTokenDef.getLocation().getEndOffset( doc );
           if (nextEnd>offset){
             nextOffset=Math.max( nextOffset, offset );
           }
           int end=Math.min( offset+length,nextEnd);

           if (currentOffset+currentLength>nextOffset){
             currentTokenDef=null;
          //   HaskellUIPlugin.log( currentOffset+"->"+currentLength+">"+nextOffset, IStatus.INFO );
             return Token.EOF;
           }

           //addTokenOccurence( nextOffset, nextEnd, nextTokenDef );

           IToken nextToken=getTokenFromTokenDef( nextTokenDef);

           // Eclipse doesn't like two tokens with the same data and space in between, so make the next token starting from the end of the previous one
           // done in mergeTokens now, simpler
           //           boolean notEOF=false;
//           if (nextOffset>=offset && currentToken!=null && currentToken.getData().equals( nextToken.getData() ) &&
//               currentOffset+currentLength<nextOffset
//               ){
//             nextOffset= currentOffset+currentLength;
//             nextOffset=Math.max( offset, nextOffset );
//             notEOF=true;
//           }
//!notEOF &&
//           if (currentOffset>0 && nextOffset<=currentOffset+currentLength){
//             HaskellUIPlugin.log( (currentOffset+currentLength)+">"+nextOffset, IStatus.ERROR );
//           }


           int nextLength=end-nextOffset;
           currentLength=nextLength;
           currentOffset=nextOffset;
           currentTokenDef=nextTokenDef;
           currentToken=nextToken;
//           HaskellUIPlugin.log( currentOffset+currentLength+" / "+offset+"/"+length, IStatus.INFO );
//
//           if (currentOffset+currentLength>=offset+length)  {
//             return Token.EOF;
//           }
           if ( nextOffset>offset+length || currentLength<=0)  {
             currentTokenDef=null;
             return Token.EOF;
           }

         } catch (BadLocationException ble){
           HaskellUIPlugin.log( ble );
         }
       } else {
         return Token.EOF;
       }
    } while(currentOffset+currentLength<=offset);
//    HaskellUIPlugin.log( currentOffset+"/"+currentLength+" / "+offset+"/"+length, IStatus.INFO );
//     HaskellUIPlugin.log(String.valueOf(currentOffset>=offset && currentOffset+currentLength<=offset+length),IStatus.INFO);
    return currentToken;

  }


  /**
   * add an occurrence for a given token
   * @param s
   * @param offset
   * @param end
   * @param td
   */
  private void addTokenOccurence(final String s,final int offset,int end,final TokenDef td){
    String name=td.getName();
    if (name.equals( ITokenTypes.KEYWORD )
        || name.equals( ITokenTypes.GHC_EXTENSION_KEYWORD )
        || name.equals( ITokenTypes.IDENTIFIER_CONSTRUCTOR )
        || name.equals( ITokenTypes.IDENTIFIER_VARIABLE )
        || name.equals( ITokenTypes.SYMBOL_RESERVED )
        ){  //|| name.equals( IScionTokens.SYMBOL_SPECIAL )

      List<String> key=new LinkedList<>();
      key.add(td.getName());
      if (end>s.length()){
        end=s.length();
      }
      if (offset>end){
        return;
      }
      key.add( s.substring(offset,end) );

      while (tokenLocations.size()<offset){
        tokenLocations.add( null );
      }
      while(tokenLocations.size()<end){
        tokenLocations.add(key);
      }
      List<TokenDef> l=occurrences.get( key );
      if (l==null){
        l=new LinkedList<>();
        occurrences.put( key, l );
      }
      l.add(td);

    }
  }

  /**
   * get occurrences for the token situated at the given offset
   * @param offset
   * @return
   */
  public List<Occurrence> getOccurrences(final int offset){
    LinkedList<Occurrence> ret=new LinkedList<>();
    if (offset>0 && offset<tokenLocations.size()){
      List<String> key=tokenLocations.get( offset );
      if (key!=null){
        List<TokenDef> l=occurrences.get( key );
        if (l!=null){

          for (TokenDef td:l){
            try {
              Occurrence occ=new Occurrence( td );
              if (td.getLocation().getStartLine()!=td.getLocation().getEndLine()){
                occ=new Occurrence( td,doc );
              }
              if (occ.getLength()>0){
                ret.add(occ);
              }
            } catch (BadLocationException ble){
              // ignore
            }
          }
          return ret;
        }
      }
    }
    return ret;
  }

  /**
   * the default damage repairer does not like spaces between token with the same data, so let's merge them
   * @param tds
   * @return
   */
  private List<TokenDef> mergeTokens(final List<TokenDef> tds){
    LinkedList<TokenDef> ret=new LinkedList<>();
    TokenDef last=null;
    for (TokenDef td:tds){
      if (last!=null && td.getName().equals( last.getName() )){
        ret.removeLast();
        // clone otherwise occurrences become wrong
        last=new TokenDef( last.getName(), new Location(last.getLocation()) );
        ret.add(last);
        last.getLocation().setEndColumn( td.getLocation().getEndColumn() );
        last.getLocation().setEndLine( td.getLocation().getEndLine() );
      } else {
        ret.add(td);
        last=td;
      }
    }

    return ret;
  }

  @Override
  public void setRange( final IDocument document, final int offset, final int length ) {
    boolean changed=false;
    if( file != null ) {
      String newContents = document.get();
      if( !document.equals( doc ) || !newContents.equals( contents ) || lTokenDefs == null ) {
        doc = document;
        contents = newContents;
//        if (!checkedTabs){
//            checkedTabs=true;
//
//        }
        BWFacade f=BuildWrapperPlugin.getFacade( file.getProject() );
        if (f==null){
          f=new BWFacade();
          f.setBwPath( BuildWrapperPlugin.getBwPath() );
          f.setProject( file.getProject() );
          f.setWorkingDir(new File(file.getProject().getLocation().toOSString()));
        }

        //long t0=System.currentTimeMillis();
        if (tgt==null){
          tgt=f.write( file, contents );
        } else {
          f.write( file,tgt, contents );
        }
        //long t01=System.currentTimeMillis();
        lTokenDefs = f.tokenTypes( file);
        changed=true;
        //long t1=System.currentTimeMillis();
        //int l=ScionPlugin.getSharedScionInstance().tokenTypes( file, contents ).size();
        //long t2=System.currentTimeMillis();
        //HaskellUIPlugin.log( "bw:"+(t1-t0)+"ms ("+lTokenDefs.size()+",write: "+(t01-t0)+"ms ), scion:"+(t2-t1)+"ms ("+l+")", IStatus.INFO );
      }
    } else {
      this.doc = document;
      contents=doc.get();
      changed=true;
      /*try {
        InputStream stream = SyntaxPreviewer.class.getResourceAsStream( "preview.json" );
        // preview file
        JSONArray result = new JSONArray( ResourceUtil.readStream( stream ) );
        lTokenDefs = new ArrayList<TokenDef>( result.length() );
        for( int i = 0; i < result.length(); ++i ) {
          JSONArray arr = result.getJSONArray( i );
          lTokenDefs.add( new TokenDef( arr ) );
        }
      } catch( Exception ex ) {
        HaskellUIPlugin.log( "Could not read preview file.", ex ); //$NON-NLS-1$
      }*/
      if (contents.length()>0){
        BWFacade f=new BWFacade();
        f.setBwPath( BuildWrapperPlugin.getBwPath() );
        try {

          File file=File.createTempFile( "temp", ".hs" );
          try {
            try (Writer fw=new BufferedWriter( new OutputStreamWriter( new FileOutputStream( file ), FileUtil.UTF8 ) )) {
              fw.write( contents );
            }
            f.setCabalFile( new File(file.getParentFile(),"temp.cabal") .getAbsolutePath());
            File newP= new File(file.getParentFile(),BWFacade.DIST_FOLDER);
            newP.mkdirs();
            File newF=new File(newP,file.getName());
            file.renameTo(newF);
            file=newF;
            lTokenDefs =  f.tokenTypes( newF.getName() );
          } finally {
            file.delete();
            file.getParentFile().delete();
          }
        } catch( Exception ex ) {
          HaskellUIPlugin.log( ex.getLocalizedMessage(), ex );
        }
      } else {
        lTokenDefs=new ArrayList<>();
      }
    }

    if (changed){

      occurrences.clear();
      tokenLocations.clear();
      //String s=doc.get();
  //    long previousOffset=-1;
  //    long previousEnd=-1;
      for (TokenDef nextTokenDef:lTokenDefs){
        try {
          int nextOffset=nextTokenDef.getLocation().getStartOffset( doc );
  //        if (nextOffset<previousOffset){
  //          HaskellUIPlugin.log( "offset error: "+nextOffset+"<"+previousOffset, IStatus.ERROR );
  //        }
  //        if (nextOffset<previousEnd){
  //          HaskellUIPlugin.log( "offset error at line "+nextTokenDef.getLocation().getStartLine()+": "+nextOffset+"<"+previousEnd, IStatus.ERROR );
  //        }
          int nextEnd=nextTokenDef.getLocation().getEndOffset( doc );
  //        if (nextOffset>nextEnd){
  //          HaskellUIPlugin.log( "extent error: "+nextOffset+">"+nextEnd, IStatus.ERROR );
  //        }
  //        if (previousOffset>nextEnd){
  //          HaskellUIPlugin.log( "extent error: "+nextEnd+">"+previousOffset, IStatus.ERROR );
  //        }
          //HaskellUIPlugin.log(nextOffset+"->"+nextEnd, IStatus.INFO);
          addTokenOccurence( contents,nextOffset, nextEnd, nextTokenDef );

  //        previousOffset=nextOffset;
  //        previousEnd=nextEnd;
        } catch (BadLocationException ble){
          HaskellUIPlugin.log( ble );
        }
      }
      if( lTokenDefs != null) {
        lMergedTokenDefs=mergeTokens( lTokenDefs);
      }
    } else if (offset == this.offset+this.length){
      this.offset = offset;
      this.length = length;
      tokenDefs.previous();
      currentTokenDef = null;
      // currentToken=null;

      currentLength=0;
      currentOffset=0;
      return;
    }
    currentTokenDef = null;
    // currentToken=null;

    currentLength=0;
    currentOffset=0;

    tokenDefs = null;
    tokenDefs =lMergedTokenDefs.listIterator();
    this.offset = offset;
    this.length = length;
    //HaskellUIPlugin.log( offset+"+"+length,IStatus.INFO );
  }

  public File getTarget(){
    return tgt;
  }

  /**
   * mark task tags
   */
  public void markTaskTags(){
    if (file!=null && tags!=null && file.exists()){
      try {
        file.deleteMarkers( IMarker.TASK , true, IResource.DEPTH_ZERO );
        setRange( doc, 0, doc.getLength() );
        for (TokenDef nextTokenDef:lTokenDefs){
          if (nextTokenDef.getName().equals(ITokenTypes.DOCUMENTATION_ANNOTATION) || nextTokenDef.getName().equals(ITokenTypes.COMMENT) || nextTokenDef.getName().equals(ITokenTypes.LITERATE_COMMENT)){
            String s=nextTokenDef.getLocation().getContents( doc );
            outer:for (int a=0;a<s.length();a++){
              if (Character.isLetter( s.charAt( a ) )){
                // substring
                String orig=s.substring( a );
                // what we'll test again, maybe changed in case
                String test=orig;
                if (!caseS){
                  test=orig.toUpperCase( Locale.ENGLISH );
                }
                for (TaskTag tt:tags){
                  if (test.startsWith( tt.getName())){

                    final Map<Object,Object> attributes=nextTokenDef.getLocation().getMarkerProperties( doc );
                    attributes.put(IMarker.PRIORITY, tt.getMarkerPriority());
                    // use original text
                    attributes.put(IMarker.MESSAGE,orig);

                    /**
                     * this locks the workspace, so fire a new thread
                     */
                    new Thread(new Runnable(){
                      @Override
                      public void run() {
                        try {
                          MarkerUtilities.createMarker(file, attributes,  IMarker.TASK);
                        } catch (CoreException ex){
                          BuildWrapperPlugin.logError(UITexts.tasks_create_error, ex);
                        }
                      }
                    }).start();

                    break outer;
                  }

                }
              }
            }
          }

        }
      } catch (Exception ble){
        HaskellUIPlugin.log( ble );
      }
    }


  }

  private IToken getTokenFromTokenDef(final TokenDef td){
    IToken tok=tokenByTypes.get(td.getName());
    if (tok!=null){
      return tok;
    }
    return man.createToken( EDITOR_DEFAULT_COLOR, EDITOR_DEFAULT_BOLD );
  }

  @Override
  public void setPartialRange( final IDocument document, final int offset, final int length,
      final String contentType, final int partitionOffset ) {
    //fContentType= contentType;
    //fPartitionOffset= partitionOffset;
    if (partitionOffset > -1) {
      int delta= offset - partitionOffset;
      if (delta > 0) {
        setRange(document, partitionOffset, length + delta);
        this.offset= offset;
        return;
      }
    }
    setRange(document, offset, length);
  }

  public void dispose(){
    HaskellUIPlugin.getDefault().getPreferenceStore().removePropertyChangeListener( this );
  }

  private void getTags(){
    tags=TaskTag.getTasksTags( HaskellUIPlugin.getDefault().getPreferenceStore() );
    // we're case insensitive, let's change all to upper case
    if (tags!=null && !caseS){
      for (TaskTag tt:tags){
        tt.setName( tt.getName().toUpperCase( Locale.ENGLISH ) );
      }
    }
  }

  private void getCaseS(){
    caseS=HaskellUIPlugin.getDefault().getPreferenceStore().getBoolean( EDITOR_TASK_TAGS_CASE);
    getTags();
  }

  /* (non-Javadoc)
   * @see org.eclipse.core.runtime.Preferences.IPropertyChangeListener#propertyChange(org.eclipse.core.runtime.Preferences.PropertyChangeEvent)
   */
  @Override
  public void propertyChange( final PropertyChangeEvent arg0 ) {
    // listen to relevant property changes and update ourselves
    if (arg0.getProperty().equals( EDITOR_TASK_TAGS )){
      getTags();
    } else if (arg0.getProperty().equals( EDITOR_TASK_TAGS_CASE )){
      getCaseS();
    }
    buildTokenTypes();
  }


}
