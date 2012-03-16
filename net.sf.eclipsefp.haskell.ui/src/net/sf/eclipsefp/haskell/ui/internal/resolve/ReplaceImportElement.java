package net.sf.eclipsefp.haskell.ui.internal.resolve;

import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports.ImportsManager;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.osgi.util.NLS;

/**
 * replace an element in an import list
 * @author JP Moresmau
 *
 */
public class ReplaceImportElement extends MarkerCompletion {
  private final String oldElement;
  private final String newElement;



  public ReplaceImportElement( final String oldElement, final String newElement ) {
    super();
    this.oldElement = oldElement;
    this.newElement = newElement;
  }

  @Override
  public ICompletionProposal getCompletionProposal( final IMarker marker,
      final IDocument document ) {
    ImportsManager mgr = new ImportsManager( (IFile)marker.getResource(), document );
    int line = marker.getAttribute( IMarker.LINE_NUMBER, -1 ) - 1;
    return mgr.replaceItemInImport( oldElement,newElement, line, getLabel() );
  }

  @Override
  public String getLabel() {
    return NLS.bind(UITexts.resolve_import_element_replace,newElement);
  }

}
