// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.views.outline;

import java.util.List;
import net.sf.eclipsefp.haskell.scion.client.OutlineHandler;
import net.sf.eclipsefp.haskell.scion.types.Location;
import net.sf.eclipsefp.haskell.scion.types.OutlineDef;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
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
    viewer.setContentProvider( new OutlineCP() );
    viewer.setLabelProvider( new OutlineLabelProvider());
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
      /*if( firstElement instanceof IHaskellLanguageElement ) {
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
      }*/
      if (firstElement instanceof OutlineDef){
        OutlineDef od=(OutlineDef)firstElement;
        Location srcLoc=od.getLocation();
        if (srcLoc!=null){
          IEditorInput fei = editor.getEditorInput();
          IDocument doc = editor.getDocumentProvider().getDocument( fei );
          try {
            int offset = doc.getLineOffset( srcLoc.getStartLine() ) + srcLoc.getStartColumn();
            int length = od.getName().length();
            try {
              editor.setHighlightRange( offset, length, true );
            } catch( IllegalArgumentException iaex ) {
              editor.resetHighlightRange();
            }
          } catch( final BadLocationException badlox ) {
            // ignore
          }

        }

      }
    }
  }


  /** <p>sets the input of the outline page.</p> */
  public void setInput( final Object input ) {
	  if( input != null && input instanceof IFileEditorInput ) {
      IFileEditorInput fei = ( IFileEditorInput )input;
      IFile file = fei.getFile();
      if( file != null && file.exists() ) {
        HaskellUIPlugin.getDefault().getScionInstanceManager( file ).outline(new OutlineHandler() {

          public void outlineResult( final List<OutlineDef> outlineDefs ) {
            HaskellOutlinePage.this.input=outlineDefs;
            HaskellOutlinePage.this.update();

          }
        });
      }
    }

  }

  /** <p>updates the outline page.</p> */
  public void update() {
   getControl().getDisplay().syncExec( new Runnable(){
     public void run() {
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

   } );

  }
}