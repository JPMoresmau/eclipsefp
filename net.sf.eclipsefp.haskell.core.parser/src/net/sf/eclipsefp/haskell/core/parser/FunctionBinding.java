// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import de.leiffrenzel.fp.haskell.core.halamo.*;

/** <p>implementation for <code>IFunctionBinding</code>.</p>
  *
  * @author Leif Frenzel
  */
class FunctionBinding extends Declaration implements IFunctionBinding {

  // a dummy name in case we have nothing to display
  private static final String FUNCTION_BINDING_NAME = "function binding";
  
  private IMatch[] matches;
  
  FunctionBinding( final IHaskellLanguageElement parent, 
                   final IModule module,
                   final CompilationUnit cu,
                   final int pos ) {
    super( parent, module );
    init( cu, pos );
    mapSourceLocation( cu );
  }


  // interface methods of IFunctionBinding
  ////////////////////////////////////////
  
  public IMatch[] getMatches() {
    return matches;
  }
  
  // interface methods of IHaskellLanguageElement
  ///////////////////////////////////////////////
  
  public String getName() {
    return FUNCTION_BINDING_NAME;
  }
  
  public ISourceLocation getSourceLocation() {
    // function bindings have no source locations themselves, we return the
    // location of the first match (if any)
    ISourceLocation result = null;
    if( matches.length > 0 ) {
      result = matches[ 0 ].getSourceLocation();
    }
    return result;
  }
  
  // helping methods
  //////////////////

  private void init( final CompilationUnit cu, final int pos ) {
    int handle = cu.getHandle();
    int count = NativeParser.getFunctionBindingMatchCount( handle, pos );
    matches = new IMatch[ count ];
    for( int i = 0; i < count; i++ ) {
      String name = NativeParser.getFunctionBindingMatchName( handle, pos, i );
      Match match = new Match( getModule(), this, name );
      int line = NativeParser.getFunctionBindingMatchLine( handle, pos, i );
      int column = NativeParser.getFunctionBindingMatchColumn( handle, pos, i );
      match.setSourceLocation( new SourceLocation( line, column ) );
      matches[ i ] = match;
    }
  }
  
  private void mapSourceLocation( final CompilationUnit cu ) {
    ISourceLocation srcLoc = getSourceLocation();
    if( srcLoc != null ) {
      cu.mapSourceLocation( srcLoc, this );
    }
  }
}
