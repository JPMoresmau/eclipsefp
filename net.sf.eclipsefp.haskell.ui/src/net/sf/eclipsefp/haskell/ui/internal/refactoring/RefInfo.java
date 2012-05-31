// Copyright (c) 2007-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.refactoring;

import org.eclipse.core.resources.IFile;
import org.eclipse.ui.IEditorPart;


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
  private int line;
  private int column;
  private boolean allowEmptySelection;
  private IEditorPart targetEditor;

  public boolean isAllowEmptySelection() {
    return allowEmptySelection;
  }

  public void setAllowEmptySelection( final boolean allowEmptySelection ) {
    this.allowEmptySelection = allowEmptySelection;
  }

  public int getLine() {
    return line;
  }

  public void setLine( final int line ) {
    this.line = line;
  }

  public int getColumn() {
    return column;
  }

  public void setColumn( final int column ) {
    this.column = column;
  }

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


  public IEditorPart getTargetEditor() {
    return targetEditor;
  }


  public void setTargetEditor( final IEditorPart targetEditor ) {
    this.targetEditor = targetEditor;
  }
}
