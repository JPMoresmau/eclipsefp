// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import java.util.ArrayList;
import java.util.List;

import de.leiffrenzel.fp.haskell.core.halamo.*;

/** <p>implementation for <code>IClassDeclaration</code>.</p>
  *
  * @author Leif Frenzel
  */
class ClassDeclaration extends Declaration implements IClassDeclaration {

  private ITypeSignature[] typeSignatures;
  
  ClassDeclaration( final IHaskellLanguageElement parent,
                    final IModule module, 
                    final CompilationUnit cu, 
                    final int pos ) {
    super( parent, module );
    init( cu, pos );
  }

  
  // interface methods of IClassDeclaration
  /////////////////////////////////////////
  
  public ITypeSignature[] getTypeSignatures() {
    return typeSignatures;
  }
  
  
  // helping methods
  //////////////////
  
  private void init( final CompilationUnit cu, final int pos ) {
    int handle = cu.getHandle();
    int count = NativeParser.getClassDeclMemberCount( handle, pos );
    List list = new ArrayList();
    for( int i = 0; i < count; i++ ) {
      if( NativeParser.isTypeSigMember( handle, pos, i ) ) {
        TypeSignature sig = new TypeSignature( this, getModule() );
        int line = NativeParser.getClassDeclTypeSigLine( handle, pos, i );
        int column = NativeParser.getClassDeclTypeSigColumn( handle, pos, i );
        sig.setSourceLocation( new SourceLocation( line, column ) );
        int idCount 
          = NativeParser.getClassDeclTypeSigIdentifierCount( handle, pos, i );
        for( int j = 0; j < idCount; j++ ) {
          String identifier
            = NativeParser.getClassDeclTypeSigIdentifierName( handle, pos, i, j );
          sig.addIdentifier( identifier );
        }
        list.add( sig );
      }
      typeSignatures = new ITypeSignature[ list.size() ];
      list.toArray( typeSignatures );
    }
  }
}
