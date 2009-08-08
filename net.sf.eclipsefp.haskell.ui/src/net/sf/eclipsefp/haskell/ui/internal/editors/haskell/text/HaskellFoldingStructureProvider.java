// Copyright (c) 2004-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.projection.ProjectionAnnotation;
import org.eclipse.jface.text.source.projection.ProjectionAnnotationModel;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;

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

  void updateFoldingRegions() {
	// TODO TtC replace by something not Cohatoe-based
	/*
    ProjectionAnnotationModel model = getAnnModel();
    if( model != null ) {
      CohatoeServer server = CohatoeServer.getInstance();
      ICodeFolding codeFolding = server.createFunction( ICodeFolding.class );
      if( codeFolding != null ) {
        Set<Position> currentRegions = new HashSet<Position>();
        // TODO lf actually, we want to pass document.get() here (editor buffer)
        // TODO lf if this is too slow, use the progress monitor
        IFile file = computeFile();
        if( file != null && file.exists() ) {
          IContainer srcRoot = computeSourceRoot( file.getProject() );
          if( srcRoot != null ) {
            List<ICodeFoldingRegion> regions
            = codeFolding.performCodeFolding( srcRoot, file );
            for( ICodeFoldingRegion region: regions ) {
              int endLine = region.getEndLine() > 0 ? region.getEndLine() : 0;
              Position pos = createPosition( region.getStartLine(), endLine );
              currentRegions.add( pos );
            }
            updateFoldingRegions( model, currentRegions );
          }
        }
      }
    }
    */
  }

  private IContainer computeSourceRoot( final IProject project ) {
    IHaskellProject haskellProject = HaskellProjectManager.get( project );
    return haskellProject.getSourceFolder();
  }

  private IFile computeFile() {
    IFile result = null;
    IEditorInput input = editor.getEditorInput();
    if( input instanceof IFileEditorInput ) {
      IFile file = ( ( IFileEditorInput )input ).getFile();
      if( file.exists() ) {
        result = file;
      }
    }
    return result;
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
