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

/**
 * <p>Remove an unused import</p>
  *
  * @author JP Moresmau
 */
public class RemoveImportResolution extends MarkerCompletion {


  @Override
  public ICompletionProposal getCompletionProposal( final IMarker marker,
      final IDocument document ) {
    int line=marker.getAttribute(IMarker.LINE_NUMBER, 0);
    try {
      IRegion r=document.getLineInformation( line-1 );
      int l=r.getLength();
      String ld=document.getLineDelimiter( line-1 );
      if (ld!=null){
        l+=ld.length();
      }
      return new CompletionProposal( "", r.getOffset(), l, 0,HaskellUIImages.getImage( IImageNames.IMPORT_REMOVE ),getLabel(),null,null );       //$NON-NLS-1$
    } catch( BadLocationException ex ) {
      HaskellUIPlugin.log( ex );
    }
    return null;
  }

  @Override
  public String getLabel() {
    return UITexts.resolve_import_remove;
  }

}
