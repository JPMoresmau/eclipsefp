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
 * Resolution for adding an imported element to the list of imports.
 * @author Alejandro Serrano
 *
 */
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
    return NLS.bind( UITexts.resolve_import_add, element, place );
  }

  @Override
  public ICompletionProposal getCompletionProposal( final IMarker marker,
      final IDocument document ) {
    ImportsManager mgr = new ImportsManager( (IFile)marker.getResource(), document );
    return mgr.addImport( element, place, qualified, getLabel() );
  }

}
