// Copyright (c) 2004-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineDef;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.projection.ProjectionAnnotation;
import org.eclipse.jface.text.source.projection.ProjectionAnnotationModel;
import org.eclipse.ui.texteditor.IDocumentProvider;

/** <p>provides folding regions for documents in the Haskell editor.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellFoldingStructureProvider {

  private final HaskellEditor editor;

  public HaskellFoldingStructureProvider( final HaskellEditor editor ) {
    this.editor = editor;
  }

  public void updateFoldingRegions(final List<OutlineDef> outlineDefs) {
    ProjectionAnnotationModel model = getAnnModel();

    // And this is how we really get the editor's document without waiting for the
    // source viewer.
    IDocumentProvider docProvider = editor.getDocumentProvider();
    // may be null if we're late and editor has closed
    if (docProvider!=null){
      IDocument document = docProvider.getDocument( editor.getEditorInput() );

      if (model!=null){
        Set<Location> blocks=new HashSet<>();
        for (OutlineDef def : outlineDefs){
          // only blocks that are more than one line long can be folded
          if (def.getLocation()!=null && def.getLocation().getEndLine()>def.getLocation().getStartLine()){
            blocks.add( def.getLocation() );
            if (def.getCommentStartLine()!=null && def.getCommentStartLine()<def.getLocation().getStartLine()-1){
              try {
                  IRegion r1=document.getLineInformation( def.getCommentStartLine()-1 );
                  int end=def.getLocation().getStartOffset( document );
                  String del=document.getLineDelimiter( def.getLocation().getStartLine()-1 );
                  r1=new Region(r1.getOffset(),end-r1.getOffset()-del.length());

                  blocks.add( new Location( editor.getTitle(), document, r1 ) );

              } catch (BadLocationException ignore){
                //noop
              }
            }
          }

        }
        Set<Position> regions=new HashSet<>();
        for (Location l:blocks){
          Position p=createPosition( document, l.getStartLine(), l.getEndLine() );
          if (p!=null){
            regions.add( p );
          }
        }
        updateFoldingRegions( model, regions );
      }
    }

  }

  private Position createPosition( final IDocument document, final int startLine, final int endLine ) {
    Position result = null;
    try {
      int start = document.getLineOffset( startLine -1 );
      int endLine2=Math.min( document.getNumberOfLines()-1, endLine -1 );
      int end =   document.getLineOffset( endLine2)
                + document.getLineLength( endLine2);
      result = new Position( start, end - start );
    } catch( final BadLocationException badlox ) {
      // ignored
      System.err.println(startLine+","+endLine); //$NON-NLS-1$
      badlox.printStackTrace();
    }
    return result;
  }

  private ProjectionAnnotationModel getAnnModel() {
    Class<ProjectionAnnotationModel> cls = ProjectionAnnotationModel.class;
    return ( ProjectionAnnotationModel )editor.getAdapter( cls );
  }

  private void updateFoldingRegions( final ProjectionAnnotationModel model,
                                     final Set<Position> currentRegions ) {
    Annotation[] deletions = computeDifferences( model, currentRegions );

    Map<ProjectionAnnotation, Position> additionsMap
      = new HashMap<>();
    Iterator<Position> it = currentRegions.iterator();
    while( it.hasNext() ) {
      additionsMap.put( new ProjectionAnnotation(), it.next() );
    }

    if( deletions.length != 0 || additionsMap.size() != 0 ) {
      model.modifyAnnotations( deletions, additionsMap, new Annotation[] {} );
    }
  }

  private Annotation[] computeDifferences( final ProjectionAnnotationModel mdl,
                                           final Set<Position> current ) {
    List<ProjectionAnnotation> deletions= new ArrayList<>();
    Iterator<?> iter = mdl.getAnnotationIterator();
    while( iter.hasNext() ) {
      Object annotation = iter.next();
      if( annotation instanceof ProjectionAnnotation ) {
        Position position = mdl.getPosition( ( Annotation )annotation );
        if( current.contains( position ) ) {
          current.remove( position );
        } else {
          deletions.add( (ProjectionAnnotation) annotation );
        }
      }
    }
    return deletions.toArray( new Annotation[ deletions.size() ] );
  }
}
