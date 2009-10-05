package net.sf.eclipsefp.haskell.ui.internal.editors.cabal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.projection.ProjectionAnnotation;
import org.eclipse.jface.text.source.projection.ProjectionAnnotationModel;

class CabalFoldingStructureProvider {

	private final CabalFormEditor editor;
	private IDocument document;

	CabalFoldingStructureProvider( final CabalFormEditor editor ) {
		this.editor = editor;
	}

  void setDocument( final IDocument document ) {
		this.document = document;
	}

	void updateFoldingRegions( final PackageDescription pd ) {
		try {
			Class<ProjectionAnnotationModel> ad = ProjectionAnnotationModel.class;
      ProjectionAnnotationModel model
        = ( ProjectionAnnotationModel )editor.getAdapter( ad );
			if( model != null ) {
  			Set<Position> currentRegions = new HashSet<Position>();
  			addFoldingRegions( currentRegions, pd.getStanzas() );
  			updateFoldingRegions( model, currentRegions );
      }
		} catch( final BadLocationException badlox ) {
      // ignore
		}
	}

	private void updateFoldingRegions( final ProjectionAnnotationModel model,
                                     final Set<Position> currentRegions ) {
		Annotation[] deletions = diff( model, currentRegions );
		Map<ProjectionAnnotation, Position> adds
      = new HashMap<ProjectionAnnotation, Position>();
    Iterator<Position> it = currentRegions.iterator();
		while( it.hasNext() ) {
			adds.put( new ProjectionAnnotation(), it.next() );
    }

		if( ( deletions.length != 0 || adds.size() != 0 ) ) {
			model.modifyAnnotations( deletions, adds, new Annotation[ 0 ] );
    }
	}

	private Annotation[] diff( final ProjectionAnnotationModel model,
                             final Set<Position> current) {
		List<ProjectionAnnotation> deletions= new ArrayList<ProjectionAnnotation>();
    Iterator<?> it = model.getAnnotationIterator();
		while( it.hasNext() ) {
			Object annotation = it.next();
			if( annotation instanceof ProjectionAnnotation ) {
				Position position = model.getPosition( ( Annotation )annotation );
				if( current.contains( position ) ) {
					current.remove( position );
        } else {
					deletions.add( ( ProjectionAnnotation )annotation );
        }
			}
		}
		return deletions.toArray( new ProjectionAnnotation[ deletions.size() ] );
	}

	private void addFoldingRegions( final Set<Position> regions,
                                  final Iterable<PackageDescriptionStanza> elements )
                                                   throws BadLocationException {
		for( PackageDescriptionStanza element:elements) {
      int startLine = element.getStartLine();
			int endLine = element.getEndLine();
			if( startLine < endLine ) {
				int start = document.getLineOffset( startLine );
        int end = document.getLength();
        try {
  				end =   document.getLineOffset( endLine-1 )
                + document.getLineLength( endLine-1 );
        } catch( final BadLocationException badlox ) {
          // ignore
        }
				Position position= new Position( start, end - start );
				regions.add( position );
			  addFoldingRegions( regions, element.getStanzas() );
			}
		}
	}
}
