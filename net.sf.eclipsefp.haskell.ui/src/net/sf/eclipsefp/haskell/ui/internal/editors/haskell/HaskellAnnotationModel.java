// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Position;
import org.eclipse.ui.texteditor.MarkerUtilities;
import org.eclipse.ui.texteditor.ResourceMarkerAnnotationModel;

public class HaskellAnnotationModel extends ResourceMarkerAnnotationModel {

	public HaskellAnnotationModel( final IResource resource ) {
    super( resource );
  }

  @Override
  protected Position createPositionFromMarker( final IMarker marker ) {
    int start = MarkerUtilities.getCharStart( marker );
    int end = MarkerUtilities.getCharEnd( marker );
    int line = MarkerUtilities.getLineNumber( marker );

    if( start > end || start == -1 || end == -1 || line == -1 ) {
      return super.createPositionFromMarker( marker );
    }

    try {
      if (fDocument!=null){
        final int offset = fDocument.getLineOffset( line - 1 ) + start;
        final int length = end - start + 1;
        return new Position( offset, length );
      }
    } catch( BadLocationException ex ) {
      HaskellUIPlugin.log( ex );
    }
    return null;
  }

}
