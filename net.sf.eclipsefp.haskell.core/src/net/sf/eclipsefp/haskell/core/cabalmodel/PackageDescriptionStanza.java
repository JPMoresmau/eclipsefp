// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.core.cabalmodel;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;


/** <p>represents a stanza in a package description.</p>
  *
  * @author Leif Frenzel
  */
public abstract class PackageDescriptionStanza {
  private static class CabalSyntaxMap<V> extends LinkedHashMap<String, V>{
    /**
    *
    */
   private static final long serialVersionUID = -630958988149146289L;

   @Override
   public V get(final Object key) {
     if (key instanceof String){
       return super.get(((String)key).toLowerCase());
     } else if (key instanceof CabalSyntax){
       return super.get(((CabalSyntax)key).getCabalName().toLowerCase());
     }
     return null;
   }

 }

  private final String name;
  private final int startLine;
  private final int endLine;
  private int indent;
  private final Map<String, String> properties=new CabalSyntaxMap<String>();
  private final Map<String, ValuePosition> positions=new CabalSyntaxMap<ValuePosition>();


  PackageDescriptionStanza( final String name,
                            final int startLine,
                            final int endLine ) {
    this.name = name;
    this.startLine = startLine;
    this.endLine = endLine;
  }


  public int getIndent() {
    return indent;
  }


  public void setIndent( final int indent ) {
    this.indent = indent;
  }

  public Map<String, String> getProperties() {
    return properties;
  }


  public Map<String, ValuePosition> getPositions() {
    return positions;
  }


  /*public ValuePosition update(final CabalSyntax field,final String value){
    return update(field.getCabalName(),value);
  }

  public ValuePosition update(final String field,final String value){
    getProperties().put(field.toLowerCase(),value);
    ValuePosition vp=getPositions().get( field.toLowerCase() );
    if (vp==null){
      return new ValuePosition(endLine,endLine,0);
    }
    return vp;
  }*/

  public RealValuePosition update(final CabalSyntax field,final String value){
    getProperties().put(field.getCabalName().toLowerCase(),value);
    ValuePosition oldVP=getPositions().get( field );
    int indent=0;
    int subIndent=0;
    StringBuilder sb=new StringBuilder();
    if (oldVP==null){
      oldVP=new ValuePosition(getEndLine(),getEndLine(),0);
      Collection<ValuePosition> vps=getPositions().values();
      for (int a=0;a<getIndent();a++){
        sb.append( ' ');
      }
      sb.append( field.getCabalName() );
      sb.append( ":" ); //$NON-NLS-1$
      int spaces=4;
      if (vps.isEmpty()){
        subIndent=getIndent()+field.getCabalName().length()+5;
      } else {
        ValuePosition vp=vps.iterator().next();
        indent=subIndent=vp.getSubsequentIndent();
        spaces=indent-(field.getCabalName().length() +1);
      }
      for (int a=0;a<spaces;a++){
        sb.append( ' ');
      }
    } else {
      subIndent=oldVP.getSubsequentIndent();
      indent=oldVP.getInitialIndent();
    }

    BufferedReader br=new BufferedReader( new StringReader( value ) );
    try {

      String line=br.readLine();
      int count=0;
      while (line!=null){
        for (int a=0;count>0 && a<subIndent;a++){
          sb.append( ' ');
        }
        count++;
        sb.append( line );
        sb.append( System.getProperty( "line.separator" ) ); //$NON-NLS-1$
        line=br.readLine();
      }
      ValuePosition newVP=new ValuePosition(oldVP.getStartLine(),oldVP.getStartLine()+count,indent);
      newVP.setSubsequentIndent( subIndent );
      getPositions().put( field.getCabalName().toLowerCase(), newVP );
      return new RealValuePosition( oldVP,sb.toString());
    } catch (IOException ioe){
      return null;
    }
  }

  public String getName() {
    return name;
  }

  public int getStartLine() {
    return startLine;
  }

  public int getEndLine() {
    return endLine;
  }


  // interface methods of Object
  //////////////////////////////

  @Override
  public String toString() {
    return   getName()
           + " (line " //$NON-NLS-1$
           + startLine
           + "-" //$NON-NLS-1$
           + endLine
           + ")"; //$NON-NLS-1$
  }
}
