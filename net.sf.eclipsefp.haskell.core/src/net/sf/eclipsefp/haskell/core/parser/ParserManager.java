// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import java.util.Hashtable;
import java.util.Iterator;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IStatus;


/** <p>a singleton that managers parser extensions.</p>
  *
  * @author Leif Frenzel
  */
public class ParserManager {

  private static final String ATT_CLASS = "class"; //$NON-NLS-1$

  private static ParserManager _instance;

  private final Hashtable<String, IHaskellParser> htParsers;

  private ParserManager() {
    // prevent instantiation from outside this class
    htParsers = new Hashtable<String, IHaskellParser>();
  }

  public static synchronized ParserManager getInstance() {
    if( _instance == null ) {
      _instance = new ParserManager();
    }
    return _instance;
  }

  public IHaskellParser getParser() {
    // this is a sensitive point, every query for a parser from outside must
    // get a valid reference here, always! Therefore provide a dummy at least
    IHaskellParser result = new DefaultHaskellParser();

    Iterator<String> iterator = htParsers.keySet().iterator();
    if( iterator.hasNext() ) {
      result = htParsers.get( iterator.next() );
    }
    return result;
  }

  public void registerParser( final String parserId,
                              final IConfigurationElement element )
                                                          throws CoreException {
    Object parser = element.createExecutableExtension( ATT_CLASS );
    if( parser instanceof IHaskellParser ) {
      htParsers.put( parserId, ( IHaskellParser )parser );
    } else {
      String msg =   "Putative Haskell parser '"  //$NON-NLS-1$
                   + parserId
                   + "' must implement"  //$NON-NLS-1$
                   + IHaskellParser.class.getName();
      HaskellCorePlugin.log( msg, IStatus.ERROR );
    }
  }
}
