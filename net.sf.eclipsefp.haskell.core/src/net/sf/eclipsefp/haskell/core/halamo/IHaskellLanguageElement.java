// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.halamo;


/** <p>The super interface for all Haskell language elements represented in 
  * the model.</p> 
  * 
  * @author Leif Frenzel
  */
public interface IHaskellLanguageElement {

  /** <p>returns the identifier of this language element.</p> */
  String getName();
  /** <p>returns the compilation unit (the Haskell source file) to which
    * this language element belongs.</p> */
  ICompilationUnit getCompilationUnit();
  /** <p>returns the language element of which this 
    * <code>IHaskellLanguageElement</code> is a child element</p> */
  IHaskellLanguageElement getParent();
  // TODO this is not the ultimative solution
  // the parser determines source positions in terms of line and column
  // locations in the text; Eclipse needs to transform this into locations
  // in terms of document offset: where is the transformation done (IDocument
  // gives support with getLineOffset( int ), but there is not always an
  // IDocument available).
  // - need also to transform form he parser's natural counting to 
  // zero-counting in IDocuments
  // - another problem is the length of a language element (we get no end 
  // location from the parser)
  ISourceLocation getSourceLocation();
}