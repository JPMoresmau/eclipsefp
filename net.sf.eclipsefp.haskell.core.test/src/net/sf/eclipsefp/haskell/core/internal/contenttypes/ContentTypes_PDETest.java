// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.contenttypes;

import java.io.ByteArrayInputStream;
import net.sf.eclipsefp.haskell.core.test.TestCaseWithProject;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.content.IContentDescription;

/** tests the content detector for styles of literate Haskell
  *
  * @author Leif Frenzel
  */
public class ContentTypes_PDETest extends TestCaseWithProject {

  public void testNonHsContentType() throws CoreException {
    IFile file = project.getFile( "a.c" );
    file.create( new ByteArrayInputStream( new byte[ 0 ] ), true, null );
    assertNull( file.getContentDescription() );
  }

  public void testHaskellContentType() throws CoreException {
    IFile file = project.getFile( "a.hs" );
    file.create( new ByteArrayInputStream( new byte[ 0 ] ), true, null );
    String hsID = "net.sf.eclipsefp.haskell.contenttypes.haskell";
    assertEquals( hsID, file.getContentDescription().getContentType().getId() );
  }

  public void testBirdContentType() throws CoreException {
    IFile file = project.getFile( "a.lhs" );
    String content = "A\n> module A where \nBlabla";
    file.create( new ByteArrayInputStream( content.getBytes() ), true, null );
    String hsID = "net.sf.eclipsefp.haskell.contenttypes.literateHaskell";
    IContentDescription cd = file.getContentDescription();
    assertEquals( hsID, cd.getContentType().getId() );
    assertEquals( LiterateContentDescriber.BIRD,
                  cd.getProperty( LiterateContentDescriber.STYLE ) );
  }

  public void testLatexContentType() throws CoreException {
    IFile file = project.getFile( "a.lhs" );
    String content = "A\n\\begin{code}module A where\\end{code}\nBlabla";
    file.create( new ByteArrayInputStream( content.getBytes() ), true, null );
    String hsID = "net.sf.eclipsefp.haskell.contenttypes.literateHaskell";
    IContentDescription cd = file.getContentDescription();
    assertEquals( hsID, cd.getContentType().getId() );
    assertEquals( LiterateContentDescriber.LATEX,
                  cd.getProperty( LiterateContentDescriber.STYLE ) );
  }
}
