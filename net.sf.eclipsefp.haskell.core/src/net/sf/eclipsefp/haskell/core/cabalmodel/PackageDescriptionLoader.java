// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.core.cabalmodel;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;

/** <p>contains helping functionality for loading a {@link PackageDescription
  * PackageDescription model}.</p>
  *
  * @author Leif Frenzel
  */
public class PackageDescriptionLoader {

  // TODO lf also used in folding -> eliminate
  public static PackageDescription load( final String content ) {
    PackageDescription result = new PackageDescription();
    if( content != null && content.trim().length() > 0 ) {
      try {
        parse( content, result );
      } catch( final IOException ioex ) {
        // very unlikely
        HaskellCorePlugin.log( "Loading cabal file", ioex ); //$NON-NLS-1$
      }
    }
    return result;
  }


  // helping methods
  //////////////////

  private static void parse( final String content,
                             final PackageDescription pd ) throws IOException {
    List<StanzaInfo> stanzas = new ArrayList<StanzaInfo>();
    StanzaInfo lastStanza = new StanzaInfo();
    stanzas.add( lastStanza );

    BufferedReader br = new BufferedReader( new StringReader( content ) );
    int count = 0;
    boolean contentStarted = false;
    String line = br.readLine();
    while( line != null ) {
      count++;
      if( !isComment( line ) ) {
        contentStarted = true;
        if( isEmpty( line ) ) {
          lastStanza.setEnd( count - 1 );
          lastStanza = new StanzaInfo();
          lastStanza.setStart( count );
          stanzas.add( lastStanza );
        } else {
          lastStanza.getContent().add( line );
          lastStanza.getLines().add( count - 1 );
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
  }

  private static void applyStanzas( final List<StanzaInfo> stanzas,
                                    final PackageDescription pd ) {
    Iterator<StanzaInfo> it = stanzas.iterator();
    while( it.hasNext() ) {
      StanzaInfo info = it.next();
      if( !info.getContent().isEmpty() ) {
        pd.addStanza( create( info ) );
      }
    }
  }

  private static PackageDescriptionStanza create(  final StanzaInfo info ) {
    PackageDescriptionStanza result=null;
    /*int startLine=1;
    if( isExecutable( info.getContent() ) ) {
      result = new ExecutableStanza( getExecutableName( info.getContent() ),info.getStart(), info.getEnd()  );
    } else if( isLibrary( info.getContent() ) ) {
      result = new LibraryStanza( getLibraryName( info.getContent() ), info.getStart(), info.getEnd() );
    } else {
      result = new GeneralStanza(info.getStart(), info.getEnd() );
      startLine=0;
    }*/
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
      int line=info.getLines().get(a);
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
            pValue.append( "\n"); //$NON-NLS-1$
          }
          pValue.append( s.substring( thisIndent ) );
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

  private static String getValue( final List<String> content,
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
  }

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

  private static boolean isEmpty( final String line ) {
    return line.trim().length() == 0;
  }

  private static boolean isComment( final String line ) {
    return line.startsWith( "--" ); //$NON-NLS-1$
  }


  // inner classes
  ////////////////

  private static class StanzaInfo {

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
  }
}
