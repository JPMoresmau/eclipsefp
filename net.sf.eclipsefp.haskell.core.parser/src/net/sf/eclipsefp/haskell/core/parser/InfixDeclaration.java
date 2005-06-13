// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import java.util.ArrayList;
import java.util.List;

import de.leiffrenzel.fp.haskell.core.halamo.*;

/** <p>implementation for <code>IInfixDeclaration</code>.</p>
  *
  * @author Leif Frenzel
  */
class InfixDeclaration extends Declaration implements IInfixDeclaration {

  private final List operators;
  private int precedenceLevel;
  private int associativity;

  InfixDeclaration( final IHaskellLanguageElement parent, 
                    final IModule module,
                    final CompilationUnit cu,
                    final int declPos ) {
    super( parent, module );
    operators = new ArrayList();
    init( cu, declPos );
  }

  void setPrecedenceLevel( final int precedenceLevel ) {
    this.precedenceLevel = precedenceLevel;
  }
  
  void setAssociativity( final int associativity ) {
    checkAssociativity( associativity );
    this.associativity = associativity;
  }
  
  void addOperator( final String operator ) {
    operators.add( operator );
  }

  
  // interface methods of IHaskellLanguageElement
  ///////////////////////////////////////////////
  
  public String getName() {
    String result;
    if( getAssociativity() == ASSOC_LEFT ) {
      result = "infixl";
    } else if( getAssociativity() == ASSOC_RIGHT ) {
      result = "infixl";
    } else {
      result = "infix";
    }
    return result;
  }
  
  
  // interface methods of IInfixDeclaration
  /////////////////////////////////////////
  
  public int getAssociativity() {
    return associativity;
  }

  public int getPrecedenceLevel() {
    return precedenceLevel;
  }
  
  public String[] getOperators() {
    return ( String[] )operators.toArray( new String[ operators.size() ] );
  }
  
  
  // helping methods
  //////////////////
  
  private void checkAssociativity( final int associativity ) {
    if(    associativity != ASSOC_NONE 
        && associativity != ASSOC_LEFT 
        && associativity != ASSOC_RIGHT ) {
      String msg = "Unknown associativity: " + associativity;
      throw new IllegalArgumentException( msg );
    }
  }
  
  private void init( final CompilationUnit cu, final int declPos ) {
    int handle = cu.getHandle();
    int precLevel = NativeParser.getInfixDeclPrecedenceLevel( handle, declPos );
    setPrecedenceLevel( precLevel );
    int assoc = NativeParser.getInfixDeclAssociativity( handle, declPos );
    setAssociativity( assoc );
    int opCount = NativeParser.getInfixDeclOperatorCount( handle, declPos );
    for( int i = 0; i < opCount; i++ ) {
      String op = NativeParser.getInfixDeclOperator( handle, declPos, i );
      addOperator( op );
    }
  }
}
