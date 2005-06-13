// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import de.leiffrenzel.fp.haskell.core.halamo.*;

/** <p>implementation for <code>IDataDeclaration</code>.</p>
  *
  * @author Leif Frenzel
  */
class DataDeclaration extends Declaration implements IDataDeclaration {

  private IConstructor[] constructors;
  
  DataDeclaration( final IHaskellLanguageElement parent, 
                   final IModule module,
                   final CompilationUnit cu,
                   final int pos ) {
    super( parent, module );
    init( cu, pos );
  }
  
  
  // interface methods of IDataDeclaration
  ////////////////////////////////////////
  
  public IConstructor[] getConstructors() {
    return constructors;
  }
  
  
  // helping methods
  //////////////////
  
  private void init( final CompilationUnit cu, final int pos ) {
    int handle = cu.getHandle();
    int count = NativeParser.getDataDeclConstructorCount( handle, pos );
    constructors = new IConstructor[  count ];
    for( int i = 0; i < count; i++ ) {
      String name = NativeParser.getDataDeclConstructorName( handle, pos, i );
      Constructor con = new Constructor( getModule(), this, name );
      int line = NativeParser.getDataDeclConstructorLine( handle, pos, i );
      int column = NativeParser.getDataDeclConstructorColumn( handle, pos, i );
      con.setSourceLocation( new SourceLocation( line, column ) );
      constructors[ i ] = con;
    }
  }
}
