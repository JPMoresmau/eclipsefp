/**
 *  Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;

/**
 * a token that has a different field for content type, so we can keep the value as a TextAttribute
 *
 * @author JP Moresmau
 *
 */
public class ContentTypeToken extends Token {
  private String contentType=IDocument.DEFAULT_CONTENT_TYPE;


  public ContentTypeToken( final Object data ) {
    super( data );
  }

  public ContentTypeToken( final String contentType,final IToken tok ) {
    super( tok.getData() );
    setContentType(contentType);
  }

  public String getContentType() {
    return contentType;
  }


  public void setContentType( final String contentType ) {
    this.contentType = contentType;
  }

}
