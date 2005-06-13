// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import java.io.IOException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.*;

import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import de.leiffrenzel.fp.haskell.core.parser.IHaskellParser;
import de.leiffrenzel.fp.haskell.core.util.ResourceUtil;


/** <p>implements the Haskell parser interface and drives the native parser
  * to provide the needed functionality.</p>
  *
  * @author Leif Frenzel
  */
public class NativeParserBridge implements IHaskellParser {

  
  // interface methods of IHaskellParser
  //////////////////////////////////////

  public ICompilationUnit parse( final IFile file ) throws CoreException {
    ICompilationUnit result = null;
    try {
      String content = ResourceUtil.readStream( file.getContents() );
      NativeParser parser = NativeParser.getInstance();
      result = parser.parseCompilationUnit( content, file );
    } catch( final IOException ex ) {
      String pluginId = ParserPlugin.getPluginId();
      String message = ex.getMessage();
      IStatus status = new Status( IStatus.ERROR, pluginId, 0, message, ex );
      throw new CoreException( status );
    }
    return result;
  }

  public boolean canParse() {
    boolean result = false;
    try {
      // will throw an execption if the parser is disposed or was 
      // never initialized
      NativeParser parser = NativeParser.getInstance();
      result = ( parser != null );
    } catch( IllegalStateException ilstax ) {
      // then we know, no need to do anything
    }
    return result;
  }
}
