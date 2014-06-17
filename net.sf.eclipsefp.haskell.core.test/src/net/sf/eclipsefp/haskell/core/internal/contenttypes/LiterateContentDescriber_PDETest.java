// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.contenttypes;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import junit.framework.TestCase;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.content.IContentDescriber;
import org.eclipse.core.runtime.content.IContentDescription;
import org.eclipse.core.runtime.content.IContentType;

/** tests the content detector for styles of literate Haskell
  *
  * @author Leif Frenzel
  */
public class LiterateContentDescriber_PDETest extends TestCase {

  public void testEmpty() throws IOException {
    LiterateContentDescriber ld = new LiterateContentDescriber();
    FTContentDescription description = new FTContentDescription();
    int result = ld.describe( stream( "" ), description );
    // treat as Bird as long as no latex sequences have been detected
    assertEquals( IContentDescriber.VALID, result );
    assertBird( description );
  }

  public void testNonEmpty() throws IOException {
    LiterateContentDescriber ld = new LiterateContentDescriber();
    FTContentDescription description = new FTContentDescription();
    int result = ld.describe( stream( "Blabla" ), description );
    // treat as Bird as long as no latex sequences have been detected
    assertEquals( IContentDescriber.VALID, result );
    assertBird( description );
  }

  public void testDetectedBirdStyle() throws IOException {
    LiterateContentDescriber ld = new LiterateContentDescriber();
    String content = "Blabla\n> module A where\nBlabla";
    FTContentDescription description = new FTContentDescription();
    int result = ld.describe( stream( content ), description );
    assertEquals( IContentDescriber.VALID, result );
    assertBird( description );
  }

  public void testDetectedLatexStyle() throws IOException {
    LiterateContentDescriber ld = new LiterateContentDescriber();
    String content = "\\begin{code}\nmodule Demo where\n\\end{code}\n";
    FTContentDescription description = new FTContentDescription();
    int result = ld.describe( stream( content ), description );
    assertEquals( IContentDescriber.VALID, result );
    assertLatex( description );
  }


  // helping functions
  ////////////////////

  private InputStream stream( final String content ) {
    return new ByteArrayInputStream( content.getBytes() );
  }

  private void assertBird( final FTContentDescription description ) {
    QualifiedName style = LiterateContentDescriber.STYLE;
    assertEquals( LiterateContentDescriber.BIRD,
                  description.getProperty( style ) );
  }

  private void assertLatex( final FTContentDescription description ) {
    QualifiedName style = LiterateContentDescriber.STYLE;
    assertEquals( LiterateContentDescriber.LATEX,
        description.getProperty( style ) );
  }


  // inner classes
  ////////////////

  private class FTContentDescription implements IContentDescription {

    private final Map<QualifiedName, Object> props
      = new HashMap<QualifiedName, Object>();


    // interface methods of IContentDescription
    ///////////////////////////////////////////

    @Override
    public String getCharset() {
      return null;
    }

    @Override
    public IContentType getContentType() {
      return null;
    }

    @Override
    public Object getProperty( final QualifiedName key ) {
      return props.get( key );
    }

    @Override
    public boolean isRequested( final QualifiedName key ) {
      return LiterateContentDescriber.STYLE.equals( key );
    }

    @Override
    public void setProperty( final QualifiedName key, final Object value ) {
      props.put( key, value );
    }
  }
}
