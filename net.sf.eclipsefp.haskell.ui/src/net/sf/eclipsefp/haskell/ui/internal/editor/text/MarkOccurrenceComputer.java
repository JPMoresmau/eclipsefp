// Copyright (c) 2007 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.editor.text;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import net.sf.eclipsefp.haskell.ui.editor.HaskellEditor;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.IAnnotationModelExtension;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.texteditor.IDocumentProvider;

/** <p>computes identifier occurrences for highlighting in a given Haskell 
  * source editor.</p>
  *
  * @author Leif Frenzel
  */
public class MarkOccurrenceComputer {

  private final HaskellEditor editor;
  private IDocument document;
  private final IMarkOccurrences markOccurrences;

  public MarkOccurrenceComputer( final HaskellEditor editor,
                                 final IMarkOccurrences markOccurrences ) {
    if( editor == null || markOccurrences == null ) {
      throw new IllegalArgumentException();
    }
    this.editor = editor;
    this.markOccurrences = markOccurrences;
  }

  public void setDocument( final IDocument document ) {
    this.document = document;
  }

  public void compute() {
    String content = document.get();
    ISelection selection = editor.getSelectionProvider().getSelection();
    if( selection instanceof ITextSelection ) {
      ITextSelection textSelection = ( ITextSelection )selection;
      try {
        IAnnotationModel model = getAnnotationModel( editor );
        if( model instanceof IAnnotationModelExtension ) {
          int offset = textSelection.getOffset();
          int line = document.getLineOfOffset( offset ); // zero-based
          int col = computeColumn( document, offset, line );
          Occurrence[] occs = markOccurrences.mark( content, line + 1, col );
          Map<Annotation, Position> map = computeAnnotations( occs );
          IAnnotationModelExtension amx = ( IAnnotationModelExtension )model;
          amx.removeAllAnnotations();
          amx.replaceAnnotations( new Annotation[ 0 ], map );
        }
      } catch( final BadLocationException badlox ) {
        // this means we could not properly get information from the
        // editor document; no point in continuing
      }
    }
  }
  
  
  // helping functions
  ////////////////////
  
  private IAnnotationModel getAnnotationModel( final HaskellEditor editor ) {
    IAnnotationModel result = null;
    if( editor != null ) {
      IDocumentProvider documentProvider = editor.getDocumentProvider();
      IEditorInput input = editor.getEditorInput();
      result = documentProvider.getAnnotationModel( input );
    }
    return result;
  }
  
  private int computeColumn( final IDocument document, 
                             final int offset, 
                             final int line ) throws BadLocationException {
    return ( offset - document.getLineOffset( line ) + 1 );
  }
  
  private List<Position> computePositions( final Occurrence[] occurrences ) {
    List<Position> result = new ArrayList<Position>();
    for( Occurrence occ: occurrences ) {
      try {
        int offs = document.getLineOffset( occ.getLine() - 1 );
        offs += occ.getColumn() - 1;
        result.add( new Position( offs, occ.getLength() ) );
      } catch( final BadLocationException badlox ) {
        // ignore that occurrence then
      }
    }
    return result;
  }
  
  private Map<Annotation, Position> computeAnnotations( final Occurrence[] occs ) {
    Map<Annotation, Position> result = new HashMap<Annotation, Position>();
    List<Position> poss = computePositions( occs );
    Iterator<Position> it = poss.iterator();
    while( it.hasNext() ) {
      Position pos = it.next();
      String key = "net.sf.eclipsefp.haskell.ui.occurrences"; //$NON-NLS-1$
      Annotation ann = new Annotation( key, false, "" ); //$NON-NLS-1$
      result.put( ann, pos );
    }
    return result;
  }
}
