// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.refactoring.internal.ui.wizards;

import net.sf.eclipsefp.haskell.refactoring.internal.core.refactorings.IRenameModuleInfo;
import net.sf.eclipsefp.haskell.refactoring.internal.ui.HaskellRefactoringUI;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.*;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;

import de.leiffrenzel.fp.haskell.core.halamo.*;
import net.sf.eclipsefp.haskell.ui.util.text.WordFinder;

/** <p>Gets the information that is needed for creating a rename refactoring
  * out of an editor.</p>
  *
  * @author Leif Frenzel
  */
public class RenameModuleInfo implements IRenameModuleInfo {

  private String text;
  private int offset;
  private IFile file;
  private String newName;
  
  private WordFinder wordFinder = new WordFinder();
  private boolean updateReferences;

  public RenameModuleInfo( final ITextEditor textEditor, 
                           final ITextSelection selection ) {
    text = selection.getText();
    offset = selection.getOffset();
    if( textEditor != null ) {
      IEditorInput editorInput = textEditor.getEditorInput();
      if( editorInput instanceof IFileEditorInput ) {
        file = ( ( IFileEditorInput )editorInput ).getFile();
      }
      init( textEditor, editorInput );
    }
  }

  
  // interface methods of IRenameInfo
  ///////////////////////////////////
  
  /** <p>sets the new name for the module to rename.</p> */
  public void setNewName( final String newName ) {
    this.newName = newName;
  }
  
  /** <p>returns the new name for the module to rename.</p> */
  public String getNewName() {
    return newName;
  }  
  
  public String getOldName() {
    return text;
  }
  
  public int getOffset() {
    return offset;
  }
  
  public IModule getModule() {
    IModule result = null;
    if( file != null ) {
      ICompilationUnit cu = Halamo.getInstance().getCompilationUnit( file );
      IModule[] modules = cu.getModules();
      // TODO
      result = modules[ 0 ];
    }
    return result;
  }

  
  public boolean isUpdateReferences() {
    return updateReferences;
  }

  public void setUpdateReferences( final boolean updateReferences ) {
    this.updateReferences = updateReferences;
  }
  

  // helping methods
  //////////////////

  private IDocument getDocument( final ITextEditor textEditor, 
                                 final IEditorInput editorInput ) {
    IDocument result = null;
    IDocumentProvider docProvider = textEditor.getDocumentProvider();
    if( docProvider != null ) {
      result = docProvider.getDocument( editorInput );
    }
    return result;
  }
  
  private void init( final ITextEditor textEditor,
                     final IEditorInput editorInput ) {
    IDocument doc = getDocument( textEditor, editorInput );
    if( doc != null ) {
      IRegion word = wordFinder.findWord( doc, offset );
      if( word != null ) {
        offset = word.getOffset();
        try {
          text = doc.get( offset, word.getLength() );
        } catch( final BadLocationException badlox ) {
          HaskellRefactoringUI.log( badlox.getMessage(), badlox );
        }
      }
    }
  }
}
