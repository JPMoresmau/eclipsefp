// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.core.cabalmodel;

import static net.sf.eclipsefp.haskell.core.util.ResourceUtil.NL;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.LinkedList;
import java.util.List;
import java.util.StringTokenizer;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;

/** <p>contains helping functionality for loading a {@link PackageDescription
  * PackageDescription model}.</p>
  *
  * @author Leif Frenzel
  */
public class PackageDescriptionLoader {

  public static PackageDescription load( final IFile file ) throws CoreException{
    PackageDescription result = new PackageDescription();
    if (file!=null && file.exists()){
      if (!file.getWorkspace().isTreeLocked()){
        file.refreshLocal( 0, new NullProgressMonitor() );
    }
      InputStream is=file.getContents();
      try {
        BufferedReader br = new BufferedReader(new InputStreamReader( is,file.getCharset() ));
        new CabalParser(result).parse(br);
      }  catch( final IOException ioex ) {
        // very unlikely
        HaskellCorePlugin.log( "Loading cabal file", ioex ); //$NON-NLS-1$
      }
    }
    return result;
  }

  // TODO lf also used in folding -> eliminate
  public static PackageDescription load( final String content ) {
    PackageDescription result = new PackageDescription();
    if( content != null && content.trim().length() > 0 ) {
      BufferedReader br = new BufferedReader( new StringReader( content ) );
      try {

        new CabalParser(result).parse( br );
      } catch( final IOException ioex ) {
        // very unlikely
        HaskellCorePlugin.log( "Loading cabal file", ioex ); //$NON-NLS-1$
      }
    }
    return result;
  }

  public static List<String> parseList(final String value,final String seps){
    List<String> ret=new LinkedList<String>();

    if (value!=null && value.length()>0){
      StringTokenizer st=new StringTokenizer( value,seps );
      while (st.hasMoreTokens()){
        String t=st.nextToken();
        if (t.length()>0){
          ret.add(t);
        }
      }
    }
    return ret;
  }

  public static List<String> parseList(final String value){
    return parseList(value, ", "+NL ); //$NON-NLS-1$

  }



  // helping methods
  //////////////////

  private static class CabalParser{
    private static final String BRACE_OPEN="{"; //$NON-NLS-1$
    private static final String BRACE_CLOSE="}"; //$NON-NLS-1$
    private static final String COLON=":"; //$NON-NLS-1$


    private PackageDescriptionStanza lastStanza=null;
    private final LinkedList<PackageDescriptionStanza> stanzaStack=new LinkedList<PackageDescriptionStanza>();

    private int currentIndent=0;

    private int count=0;
    private int empty=0;

    private String field=null;
    private final StringBuilder fieldValue=new StringBuilder();
    private ValuePosition fieldVP=null;



    private final PackageDescription pd;

    private int braces=0;

    /**
     * are we ending in NL?
     */
    private boolean gotNLAtEnd=true;

    private CabalParser( final PackageDescription pd ){
      this.pd=pd;
    }

    private final StringBuilder line=new StringBuilder();
    private int lookup=-1;
    /**
     * yes I rewrote a readLine method to know if the file ends with NL or not
     * @param br the reader
     * @return the line read, or null of the end of the file
     * @throws IOException
     */
    private String readLine( final BufferedReader br)throws IOException{
      if (lookup>-1){
        line.append( (char)lookup );
      }
      while (true){
        lookup=br.read();
        boolean ret=false;
        if (lookup==-1){
          if (line.length()>0){
            gotNLAtEnd=false;
            ret=true;
          } else {
            return null;
          }
        } else if (lookup=='\r'){
          lookup=br.read();
          if (lookup=='\n'){
            lookup=-1;
          }
          ret=true;
        } else if (lookup=='\n'){
          ret=true;
          lookup=-1;
        } else {
          line.append( (char)lookup );
          lookup=-1;
        }
        if (ret){
          String s=line.toString();
          line.setLength( 0 );
          return s;
        }
      }
    }

    private void parse( final BufferedReader br
       ) throws IOException {
      String line = readLine(br);
      while( line != null ) {
        if (!isComment( line )){
          if(! isEmpty( line ) ) {
            if (lastStanza==null){
              lastStanza=new GeneralStanza(count);
            }
            int indent=getIndent(line);
            if (indent>currentIndent && field!=null && fieldVP!=null){
              addFieldLine(line, indent);
            } else if (line.trim().equals(BRACE_OPEN)){
              braces++;
            } else {
              String trimmed=line.trim();
              if (trimmed.startsWith( BRACE_CLOSE )){
                braces--;
                addField();
                popStanza();
                if (trimmed.length()==BRACE_CLOSE.length()){
                  line=br.readLine();
                  continue;
                }
                line=line.substring( line.indexOf( BRACE_CLOSE )+1 );

              }

              currentIndent=indent;
              addField();

              if (braces==0){
                while (indent<lastStanza.getIndent() && stanzaStack.size()>0){
                  popStanza();
                }
              }

              String start=getSectionHeader( line );
              CabalSyntax cs=CabalSyntax.sections.get( start );
              if (cs != null){
                String name=null;
                if (line.length()>start.length()+indent){
                  name=line.substring( start.length()+indent).trim();
                }
                if (name!=null && name.endsWith( BRACE_OPEN )){
                  braces++;
                  name=name.substring( 0,name.length()-1 ).trim();
                }
                addStanza(cs,name);
                //lastStanza=new PackageDescriptionStanza(cs,name,count);
              } else {
                int ix=line.indexOf(COLON );
                if (ix>-1){
                  field=line.substring( 0,ix ).trim().toLowerCase();
                  int initialIndent=ix+1;
                  int subsequentIndent=initialIndent;

                  if (ix<line.length()){
                    while (initialIndent<line.length() && Character.isWhitespace( line.charAt( initialIndent ))){
                      if ( line.charAt( initialIndent )=='\t'){
                        subsequentIndent+=3;
                      }
                      initialIndent++;
                      subsequentIndent++;
                    }
                    fieldValue.append( line.substring( initialIndent ).trim() );
                  }
                  fieldVP=new ValuePosition();
                  fieldVP.setStartLine( count );
                  fieldVP.setInitialIndent( initialIndent );
                  fieldVP.setSubsequentIndent( subsequentIndent );
                  if (lastStanza.getProperties().isEmpty()){
                    lastStanza.setIndent( indent );
                  }
                }
              }
            }
            empty=0;
          } else {
            empty++;
          }
        }
        line = readLine(br);
        count++;

      }
      if (lastStanza!=null){
        addField();
        while (stanzaStack.size()>0){
          popStanza();
        }
        if (!gotNLAtEnd){
          lastStanza.needNL=true;
        }
        addStanza(null,null);
      }
    }

    private void addField() {
      if(lastStanza!=null && field!=null){
        // fieldValue.length()>0 &&
        if(fieldVP!=null){
          lastStanza.getProperties().put( field, fieldValue.toString() );
          fieldVP.setEndLine( count - empty);
          lastStanza.getPositions().put( field, fieldVP );
          fieldValue.setLength( 0 );
          fieldVP=null;
        }
        field=null;
      }
    }

    private void addStanza(final CabalSyntax type,final String name){
      if (type!=null && (type.equals( CabalSyntax.SECTION_IF ) || type.equals( CabalSyntax.SECTION_ELSE ))){
        stanzaStack.addLast( lastStanza );
      } else {
        lastStanza.setEndLine( count-empty );
        pd.getStanzas().add(lastStanza);
      }
      lastStanza=new PackageDescriptionStanza(type,name,count);
    }

    private void popStanza(){
     if (stanzaStack.size()>0){
       PackageDescriptionStanza st=stanzaStack.removeLast();
       lastStanza.setEndLine( count-empty );
       st.getStanzas().add(lastStanza);
       lastStanza=st;
     }
    }

    private void addFieldLine(final String line,final int indent){
      if (fieldValue.length()>0){
        fieldValue.append( NL);
      }
      String val=line.substring( indent ) ;
      if (val.trim().equals( "." )){ //$NON-NLS-1$
        val=""; //$NON-NLS-1$
      }
      fieldValue.append( val.trim() );
      fieldVP.setSubsequentIndent( indent );
    }

    private static int getIndent(final String line){
      int a=0;
      while (line.charAt( a )==' ' && a<line.length()){
        a++;
      }
      return a;
    }

    private static boolean isEmpty( final String line ) {
      return line.trim().length() == 0;
    }

    private static boolean isComment( final String line ) {
      return line.trim().startsWith( "--" ); //$NON-NLS-1$
    }

    private static String getSectionHeader(final String line ) {
      String section=line.trim().toLowerCase();
      int ix=section.indexOf( ' ' );
      if (ix>-1){
        section=section.substring( 0,ix );
      }
      return section;
    }
  }




 /* private static void parseOld( final BufferedReader br,
                             final PackageDescription pd ) throws IOException {
    List<StanzaInfo> stanzas = new ArrayList<StanzaInfo>();
    StanzaInfo lastStanza = new StanzaInfo();
    stanzas.add( lastStanza );

    Set<String> sections=new HashSet<String>();
    for (CabalSyntax cs:CabalSyntax.values() ){
      if (cs.isSectionHeader()){
        sections.add(cs.getCabalName().toLowerCase());
      }
    }

    int count = 0;
    boolean contentStarted = false;
    String line = br.readLine();
    int empty=1;
    while( line != null ) {
      count++;
      if( !isComment( line ) ) {
        contentStarted = true;
        if(! isEmpty( line ) ) {
          if (empty>1 && isSectionHeader(line,sections)){
            if (getSectionHeader(line).equals( CabalSyntax.SECTION_IF )){

            }
            lastStanza.setEnd( count - empty );
            lastStanza = new StanzaInfo();
            lastStanza.setStart( count-1 );
            stanzas.add( lastStanza );
          }

          lastStanza.getContent().add( line );
          lastStanza.getLines().add( new Integer(count - 1) );
          empty=1;
        } else {
          empty++;
        }
      } else if( !contentStarted ) {
        lastStanza.setStart( lastStanza.getStart() + 1 );
      }
      line = br.readLine();
    }
    if( !lastStanza.getContent().isEmpty() ) {
      // then we had not yet a chance to set the end line
      lastStanza.setEnd( count );
    }
    applyStanzas( stanzas, pd );
  }*/

  /*private static boolean isSectionHeader(final String line, final Set<String> sections ) {
    return sections.contains(getSectionHeader(line));
  }*/



  /*private static void applyStanzas( final List<StanzaInfo> stanzas,
                                    final PackageDescription pd ) {
    Iterator<StanzaInfo> it = stanzas.iterator();
    while( it.hasNext() ) {
      StanzaInfo info = it.next();
      if( !info.getContent().isEmpty() ) {
        pd.addStanza( create( info ) );
      }
    }
  }*/

  /*private static PackageDescriptionStanza create(  final StanzaInfo info ) {
    PackageDescriptionStanza result=null;
    /int startLine=1;
    if( isExecutable( info.getContent() ) ) {
      result = new ExecutableStanza( getExecutableName( info.getContent() ),info.getStart(), info.getEnd()  );
    } else if( isLibrary( info.getContent() ) ) {
      result = new LibraryStanza( getLibraryName( info.getContent() ), info.getStart(), info.getEnd() );
    } else {
      result = new GeneralStanza(info.getStart(), info.getEnd() );
      startLine=0;
    }/
    int startLine=0;

    if (info.getContent()!=null && info.getContent().size()>0){
      String s=info.getContent().get( 0 ).trim();
      for (CabalSyntax cs:CabalSyntax.values()){
        if (cs.isSectionHeader()){
          if (s.toLowerCase().startsWith( cs.getCabalName().toLowerCase())){
            String name=s.substring( cs.getCabalName().length() ).trim();
            if (name.endsWith( "{" )){ //$NON-NLS-1$
              name=name.substring( 0,name.length()-1 ).trim();
            }

            if(cs.equals( CabalSyntax.SECTION_LIBRARY )){
              name=null;
            }
            startLine=1;
            result = new PackageDescriptionStanza(cs,name,info.getStart(), info.getEnd() );
          }
        }
      }
    }
    if (result==null){
      result=new GeneralStanza( info.getStart(), info.getEnd() );
    }
    parseProperties(info,result,startLine);
    return result;
  }

  private static void parseProperties( final StanzaInfo info,
      final PackageDescriptionStanza result, final int startLine ) {
    String pName=null;
    StringBuilder pValue=new StringBuilder();
    ValuePosition vp=null;


    for (int a=startLine;a<info.getContent().size();a++){
      String s=info.getContent().get( a );
      int line=info.getLines().get(a).intValue();
      if (vp==null){
        vp=new ValuePosition();
        vp.setStartLine( line );
        vp.setEndLine( line+1 );
      } else {
        vp.setEndLine( line );
      }
      int thisIndent=0;
      while (thisIndent<s.length() && Character.isWhitespace( s.charAt( thisIndent ))){
        thisIndent++;
      }
      if (a==startLine){
       result.setIndent( thisIndent );
      }
      if (thisIndent>result.getIndent()){
        if (thisIndent<s.length()){
          if (pValue.length()>0){
            pValue.append( NL); //$NON-NLS-1$
          }
          String val=s.substring( thisIndent ) ;
          if (val.trim().equals( "." )){ //$NON-NLS-1$
            val=""; //$NON-NLS-1$
          }
          pValue.append(val );
          vp.setSubsequentIndent( thisIndent );
        }
      } else {
        if (pName!=null && pValue.length()>0){
          result.getProperties().put( pName.toLowerCase(), pValue.toString() );
          result.getPositions().put(pName.toLowerCase(),vp);
          vp=new ValuePosition();
          vp.setStartLine( line );
          vp.setEndLine( line +1 );
          pValue.setLength( 0 );
          pName=null;
        }
        int ix=s.indexOf( ":" ); //$NON-NLS-1$
        if (ix>-1){
          pName=s.substring( 0,ix ).trim();
          ix++;
          int valIndent=ix;
          int subIndent=ix;
          while (ix<s.length() && Character.isWhitespace( s.charAt( ix ))){
            valIndent++;
            subIndent++;
            if (s.charAt( ix )=='\t'){
              subIndent+=3;
            }
            ix++;
          }
          vp.setInitialIndent( valIndent );
          vp.setSubsequentIndent( subIndent );
          pValue.append(s.substring( ix ));
        }
      }
    }
    if (pName!=null && pValue.length()>0){
      result.getProperties().put( pName.toLowerCase(), pValue.toString() );
      result.getPositions().put(pName.toLowerCase(),vp);
      pValue.setLength( 0 );
    }
  }
*/

  /*private static String getPlainName( final List<String> content ) {
    String result = getValue( content, CabalSyntax.FIELD_NAME );
    if( result == null ) {
      result = "Unknown"; //$NON-NLS-1$
    }
    return result;
  }*/

  /*private static String getLibraryName( final List<String> content ) {
    return getValue( content, CabalSyntax.FIELD_NAME.toString() );
  }*/

  /*private static String getExecutableName( final List<String> content ) {
    //return getValue( content, CabalSyntax.FIELD_EXECUTABLE );
    if (content.size()==0){
      return null;
    }
    String s=content.get( 0 );
    if (s.toLowerCase().startsWith( CabalSyntax.SECTION_EXECUTABLE.toString() )){
      if (s.length()>CabalSyntax.SECTION_EXECUTABLE.toString().length() ){
        return s.substring( CabalSyntax.SECTION_EXECUTABLE.toString().length() ).trim();
      }
      return ""; //$NON-NLS-1$
    }
    return s;
  }*/

  /*private static String getValue( final List<String> content,
                                  final String fieldName ) {
    String result = null;
    Iterator<String> it = content.iterator();
    while( result == null && it.hasNext() ) {
      String next = it.next();
      String nextNoCase = next.trim().toLowerCase();
      if( nextNoCase.startsWith( fieldName.toLowerCase() ) ) {
        int index = next.indexOf( ':' ) + 1;
        result = next.substring( index ).trim();
      }
    }
    return result;
  }*/

  /*private static boolean isLibrary( final List<String> content ) {
    return containsLineStart( content, CabalSyntax.SECTION_LIBRARY.getCabalName());
  }

  private static boolean isExecutable( final List<String> content ) {
    return containsLineStart( content, CabalSyntax.SECTION_EXECUTABLE.getCabalName() );
  }

  private static boolean containsLineStart( final List<String> content,
                                            final String start ) {
    boolean result = false;
    Iterator<String> it = content.iterator();
    while( !result && it.hasNext() ) {
      String next = it.next().trim().toLowerCase();
      result = next.startsWith( start.toLowerCase() );
    }
    return result;
  }*/


  // inner classes
  ////////////////

 /* private static class StanzaInfo {

    private final List<String> content = new ArrayList<String>();
    private final List<Integer> lines=new ArrayList<Integer>();
    private int start = 0;
    private int end = 1;

    StanzaInfo() {
      // NOOP
    }

    List<String> getContent() {
      return content;
    }

    public List<Integer> getLines() {
      return lines;
    }

    void setEnd( final int end ) {
      this.end = end;
    }

    int getEnd() {
      return end;
    }

    void setStart( final int start ) {
      this.start = start;
    }

    int getStart() {
      return start;
    }
  }*/
}
