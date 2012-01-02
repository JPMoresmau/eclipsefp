/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.resolve;

import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports.ImportsManager;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.osgi.util.NLS;

/**
 * Resolution for removing a redundant item in an import.
 * @author Alejandro Serrano
 *
 */
public class RemoveRedundantElementInImportResolution extends MarkerCompletion {

  String element;

  public RemoveRedundantElementInImportResolution(final String element) {
    this.element = element;
  }

  public String getLabel() {
    return NLS.bind( UITexts.resolve_import_remove_part, element );
  }

  @Override
  public ICompletionProposal getCompletionProposal( final IMarker marker,
      final IDocument document ) {
    ImportsManager mgr = new ImportsManager( (IFile)marker.getResource(), document );
    int line = marker.getAttribute( IMarker.LINE_NUMBER, -1 ) - 1;
    return mgr.removeItemInImport( element, line, getLabel() );
  }

}
