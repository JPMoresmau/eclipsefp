package net.sf.eclipsefp.haskell.ui.internal.resolve;

import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.osgi.util.NLS;


public class AddImportResolution extends MarkerCompletion {

  String element;
  String place;
  String qualified;

  public AddImportResolution(final String element, final String place, final String qualified) {
    this.element = element;
    this.place = place;
    this.qualified = qualified;
  }

  public String getLabel() {
    return NLS.bind( UITexts.resolve_import_add, place );
  }

  @Override
  public ICompletionProposal getCompletionProposal( final IMarker marker,
      final IDocument document ) {
    // TODO Auto-generated method stub
    return null;
  }

}
