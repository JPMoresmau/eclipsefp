/**
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.resolve;

import java.util.List;
import java.util.Locale;
import net.sf.eclipsefp.haskell.buildwrapper.types.GhcMessages;

/**
 * Encapsulate parsing out of scope messages
 *
 * @author JP Moresmau
 *
 */
public class ResolutionSuggestion {
  /**
   * out of scope full name
   */
  private String outOfScope;
  /**
   * out of scope short name
   */
  private String outOfScopeName;
  /**
   * out of scope qualifier or null if not qualified
   */
  private String outOfScopeQualifier;
  /**
   * suggestions from message, potentially null
   */
  List<String> suggestions=null;

  public ResolutionSuggestion(final String msg,final int ix){
    this(msg,ix,msg.toLowerCase( Locale.ENGLISH ));
  }

  public ResolutionSuggestion(final String msg,final int ix,String msgL){
    int start = msgL.indexOf( ':',ix);
    int l=msgL.indexOf( "\n",0);
    if (l>-1){
      String sug=msg.substring( l );
      suggestions=ReplaceTextResolution.getSuggestionsFromGHCMessage( sug,msgL.substring( l ) );
      msgL=msgL.substring( 0,l );
    }
    int end =msgL.length();
    outOfScope = msg.substring( start + 1, end ).trim();
    // type or class is specified
    if (outOfScope.startsWith( GhcMessages.TYPE_OR_CLASS )){
      outOfScope=outOfScope.substring( GhcMessages.TYPE_OR_CLASS.length() ).trim();
    }
    if (outOfScope.startsWith( "`" ) && outOfScope.endsWith( "'" )){
      outOfScope=outOfScope.substring( 1,outOfScope.length()-1 );
    }

    int pointPos = outOfScope.lastIndexOf( '.' );
    if (pointPos != -1) {
      outOfScopeName = outOfScope.substring( pointPos + 1 );
      outOfScopeQualifier = outOfScope.substring( 0, pointPos );
    } else {
      outOfScopeName = outOfScope;
      outOfScopeQualifier = null;
    }
  }


  public String getOutOfScope() {
    return outOfScope;
  }

  public String getOutOfScopeName() {
    return outOfScopeName;
  }


  public String getOutOfScopeQualifier() {
    return outOfScopeQualifier;
  }


  public List<String> getSuggestions() {
    return suggestions;
  }
}
