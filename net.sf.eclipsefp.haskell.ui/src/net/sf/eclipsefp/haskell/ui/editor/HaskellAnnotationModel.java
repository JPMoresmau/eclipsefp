package net.sf.eclipsefp.haskell.ui.editor;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Position;
import org.eclipse.ui.texteditor.MarkerUtilities;
import org.eclipse.ui.texteditor.ResourceMarkerAnnotationModel;

public class HaskellAnnotationModel extends ResourceMarkerAnnotationModel {

	public HaskellAnnotationModel(IResource resource) {
		super(resource);
	}

	@Override
	protected Position createPositionFromMarker(IMarker marker) {

		int start= MarkerUtilities.getCharStart(marker);
		int end= MarkerUtilities.getCharEnd(marker);
		int line = MarkerUtilities.getLineNumber(marker);

		if (start > end || start == -1 || end == -1 || line == -1) {
			return null;
		}

		try {
			final int offset = fDocument.getLineOffset(line - 1) + start;
			final int length = end - start + 1;
			return new Position(offset, length);
		} catch (BadLocationException ex) {
			return null;
		}
	}
	
}
