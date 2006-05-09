// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import java.util.Hashtable;
import java.util.Iterator;

import org.eclipse.core.runtime.*;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;


/** <p>a singleton that managers parser extensions.</p>
  * 
  * @author Leif Frenzel
  */
public class ParserManager {

  private static ParserManager _instance;
  
  private Hashtable htParsers;
  
  private ParserManager() {
    // prevent instantiation from outside this class
    htParsers = new Hashtable();
  }
  
  public static synchronized ParserManager getInstance() {
    if( _instance == null ) {
      _instance = new ParserManager();
    }
    return _instance;
  }
  
  // TODO cache, query
  public IHaskellParser getParser() {
    // this is a sensitive point, every query for a parser from outside must 
    // get a valid reference here, always! Therefore provide a dummy at least
    IHaskellParser result = new DefaultHaskellParser();

    Iterator iterator = htParsers.keySet().iterator();
    if( iterator.hasNext() ) {
      result = ( IHaskellParser )htParsers.get( iterator.next() );
    }
    return result;
  }

  public void registerParser( final String parserId, 
                              final IConfigurationElement element ) 
                                                          throws CoreException {
    Object parser = element.createExecutableExtension( "class" );
    if( parser instanceof IHaskellParser ) {
      htParsers.put( parserId, parser );
    } else {
      String msg =   "Putative Haskell parser '" 
                   + parserId 
                   + "' must implement" 
                   + IHaskellParser.class.getName();
      HaskellCorePlugin.log( msg, IStatus.ERROR );
    }
  }
}
