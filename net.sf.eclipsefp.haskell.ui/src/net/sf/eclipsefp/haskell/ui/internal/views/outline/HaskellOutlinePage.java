// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.views.outline;

import net.sf.eclipsefp.haskell.core.halamo.IHaskellLanguageElement;
import net.sf.eclipsefp.haskell.core.halamo.ISourceLocation;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.internal.views.common.TreeElementCP;
import net.sf.eclipsefp.haskell.ui.internal.views.common.TreeElementLP;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;


/** <p>The outline page for the Haskell editor.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellOutlinePage extends ContentOutlinePage {

  private Object input;
  private final HaskellEditor editor;

  public HaskellOutlinePage( final HaskellEditor textEditor ) {
    this.editor = textEditor;
  }

  @Override
  public void createControl( final Composite parent ) {
    super.createControl( parent );

    TreeViewer viewer = getTreeViewer();
    viewer.setContentProvider( new TreeElementCP() );
    viewer.setLabelProvider( new TreeElementLP() );
    viewer.addSelectionChangedListener( this );

    if( input != null ) {
      viewer.setInput( input );
    }
  }

  @Override
  public void selectionChanged( final SelectionChangedEvent event ) {
    super.selectionChanged( event );

    ISelection selection= event.getSelection();
    if( selection.isEmpty() ) {
      editor.resetHighlightRange();
    } else {
      IStructuredSelection sel = ( IStructuredSelection )selection;
      Object firstElement = sel.getFirstElement();
      if( firstElement instanceof IHaskellLanguageElement ) {
        IHaskellLanguageElement elem = ( IHaskellLanguageElement )firstElement;
        IEditorInput fei = editor.getEditorInput();
        IDocument doc = editor.getDocumentProvider().getDocument( fei );
        ISourceLocation srcLoc = elem.getSourceLocation();
        if( srcLoc != null ) {
          int offset = -1;
          try {
            offset = doc.getLineOffset( srcLoc.getLine() ) + srcLoc.getColumn();
          } catch( final BadLocationException badlox ) {
            // ignore
          }
          int length = elem.getName().length();
          try {
            editor.setHighlightRange( offset, length, true );
          } catch( IllegalArgumentException iaex ) {
            editor.resetHighlightRange();
          }
        }
      }
    }
  }

  /** <p>sets the input of the outline page.</p> */
  public void setInput( final Object input ) {
	// TODO replace by something not Cohatoe-based
	/*
    if( input != null && input instanceof IFileEditorInput ) {
      IFileEditorInput fei = ( IFileEditorInput )input;
      IFile file = fei.getFile();
      if( file != null && file.exists() ) {
        IDocument doc = editor.getDocument();
        CohatoeServer server = CohatoeServer.getInstance();
        IHaskellOutline fun = server.createFunction( IHaskellOutline.class );
        if( fun != null ) {
          this.input = fun.computeOutline( doc.get() );
          update();
        }
      }
    }
    */
  }

  /** <p>updates the outline page.</p> */
  public void update() {
    TreeViewer viewer = getTreeViewer();
    if( viewer != null ) {
      Control control= viewer.getControl();
      if( control != null && !control.isDisposed() ) {
        control.setRedraw( false );
        viewer.setInput( input );
        viewer.expandToLevel( 2 );
        control.setRedraw( true );
      }
    }
  }
}