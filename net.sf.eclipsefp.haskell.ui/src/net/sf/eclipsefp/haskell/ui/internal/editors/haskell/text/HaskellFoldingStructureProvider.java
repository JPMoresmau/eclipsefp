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
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.ICodeFolding.ICodeFoldingRegion;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.projection.ProjectionAnnotation;
import org.eclipse.jface.text.source.projection.ProjectionAnnotationModel;
import de.leiffrenzel.cohatoe.server.core.CohatoeServer;

/** <p>provides folding regions for documents in the Haskell editor.</p>
  *
  * @author Leif Frenzel
  */
class HaskellFoldingStructureProvider {

  private final HaskellEditor editor;
  private IDocument document;
  private IProgressMonitor progressMonitor;

  HaskellFoldingStructureProvider( final HaskellEditor editor ) {
    this.editor = editor;
  }

  void setProgressMonitor( final IProgressMonitor progressMonitor ) {
    this.progressMonitor = progressMonitor;
  }

  void setDocument( final IDocument document ) {
    this.document = document;
  }

  void updateFoldingRegions( final String buffer ) {
    ProjectionAnnotationModel model = getAnnModel();
    if( model != null ) {
      CohatoeServer server = CohatoeServer.getInstance();
      ICodeFolding codeFolding = server.createFunction( ICodeFolding.class );
      if( codeFolding != null ) {
        Set<Position> currentRegions = new HashSet<Position>();
        List<ICodeFoldingRegion> regions
          = codeFolding.performCodeFolding( buffer );
        for( ICodeFoldingRegion region: regions ) {
          int endLine = region.getEndLine() > 0 ? region.getEndLine() : 0;
          Position pos = createPosition( region.getStartLine(), endLine );
          currentRegions.add( pos );
        }
        updateFoldingRegions( model, currentRegions );
      }
    }
  }

  private Position createPosition( final int startLine, final int endLine ) {
    Position result = null;
    try {
      int start = document.getLineOffset( adjustForZero( startLine ) );
      int end =   document.getLineOffset( endLine )
                + document.getLineLength( endLine );
      result = new Position( start, end - start );
    } catch( final BadLocationException badlox ) {
      // ignored
      badlox.printStackTrace();
    }
    return result;
  }

  private int adjustForZero( final int line ) {
    return line > 0 ? line - 1 : 0;
  }

  private ProjectionAnnotationModel getAnnModel() {
    Class<ProjectionAnnotationModel> cls = ProjectionAnnotationModel.class;
    return ( ProjectionAnnotationModel )editor.getAdapter( cls );
  }

  private void updateFoldingRegions( final ProjectionAnnotationModel model,
                                     final Set<Position> currentRegions ) {
    Annotation[] deletions = computeDifferences( model, currentRegions );

    Map<ProjectionAnnotation, Position> additionsMap
      = new HashMap<ProjectionAnnotation, Position>();
    Iterator<Position> it = currentRegions.iterator();
    while( it.hasNext() ) {
      additionsMap.put( new ProjectionAnnotation(), it.next() );
    }

    if(    ( deletions.length != 0 || additionsMap.size() != 0 )
        && !isCancelled() ) {
      model.modifyAnnotations( deletions, additionsMap, new Annotation[] {} );
    }
  }

  private boolean isCancelled() {
    return progressMonitor != null && ( progressMonitor.isCanceled() );
  }

  private Annotation[] computeDifferences( final ProjectionAnnotationModel mdl,
                                           final Set<Position> current ) {
    List<ProjectionAnnotation> deletions= new ArrayList<ProjectionAnnotation>();
    Iterator iter = mdl.getAnnotationIterator();
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
