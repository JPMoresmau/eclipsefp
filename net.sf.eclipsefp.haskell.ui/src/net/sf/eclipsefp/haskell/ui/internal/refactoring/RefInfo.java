// Copyright (c) 2007-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.refactoring;

import org.eclipse.core.resources.IFile;


/** <p>an info object that holds the information that is passed from
  * the user to the refactoring.</p>
  *
  * @author Leif Frenzel
  */
public class RefInfo {

  // the offset of the selected code portion
  private int offset;
  // the file that contains the code to be refactored
  private IFile sourceFile;
  // the selected code
  private String text;

  // interface methods of IRenamePropertyInfo
  ///////////////////////////////////////////

  public int getOffset() {
    return offset;
  }

  public void setOffset( final int offset ) {
    this.offset = offset;
  }

  public IFile getSourceFile() {
    return sourceFile;
  }

  public void setSourceFile( final IFile sourceFile ) {
    this.sourceFile = sourceFile;
  }

  public String getText() {
    return text;
  }

  public void setText( final String text ) {
    this.text = text;
  }
}
