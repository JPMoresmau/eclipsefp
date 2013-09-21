/**
 *  Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import org.eclipse.jface.text.rules.FastPartitioner;
import org.eclipse.jface.text.rules.IToken;

/**
 * Partitioner for Haskell, based on ScionTokenScanner
 *
 * @author JP Moresmau
 *
 */
public class HaskellDocumentPartitioner extends FastPartitioner {


  public HaskellDocumentPartitioner( final ScionTokenScanner scanner,
      final String[] legalContentTypes ) {
    super( scanner, legalContentTypes );

  }

  public ScionTokenScanner getScanner(){
    return (ScionTokenScanner)fScanner;
  }


  @Override
  protected String getTokenContentType( final IToken token ) {
    if (token instanceof ContentTypeToken){
      return ((ContentTypeToken)token).getContentType();
    }
    return super.getTokenContentType( token );
  }
}
