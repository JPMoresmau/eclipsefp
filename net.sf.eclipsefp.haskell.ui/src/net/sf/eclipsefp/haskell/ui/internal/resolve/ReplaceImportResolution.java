package net.sf.eclipsefp.haskell.ui.internal.resolve;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.osgi.util.NLS;

/**
 * <p>Replace import by another</p>
  *
  * @author JP Moresmau
 */
public class ReplaceImportResolution extends MarkerCompletion {
  private final String newImport;

  public ReplaceImportResolution( final String newImport ) {
    super();
    this.newImport = newImport;
  }

  @Override
  public ICompletionProposal getCompletionProposal( final IMarker marker,
      final IDocument document ) {
    int line=marker.getAttribute(IMarker.LINE_NUMBER, 0);
    try {
      IRegion r=document.getLineInformation( line-1 );
      int l=r.getLength();
      return new CompletionProposal( newImport, r.getOffset(), l, 0,HaskellUIImages.getImage( IImageNames.IMPORT ),getLabel(),null,null );
    } catch( BadLocationException ex ) {
      HaskellUIPlugin.log( ex );
    }
    return null;
  }

  public String getLabel() {
    return NLS.bind(UITexts.resolve_import_replace,newImport);
  }

}
