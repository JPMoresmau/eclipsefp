package net.sf.eclipsefp.haskell.ui.internal.resolve;

import net.sf.eclipsefp.haskell.scion.types.GhcMessages;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;

/**
 * <p>Add type signature</p>
  *
  * @author JP Moresmau
 */
public class MissingTypeWarningResolution extends MarkerCompletion {

  public String getLabel() {
    return UITexts.resolve_missingtype;
  }

  @Override
  public ICompletionProposal getCompletionProposal( final IMarker marker,final IDocument document){
    String msg=marker.getAttribute(IMarker.MESSAGE,""); //$NON-NLS-1$
    String toSearch=GhcMessages.WARNING_INFERREDTYPE_START;
    int ix=msg.toLowerCase().indexOf(  toSearch);
    String type=msg.substring(ix+toSearch.length()).trim();

    int line=marker.getAttribute(IMarker.LINE_NUMBER, 0);
    try {

      int offset=document.getLineOffset( line-1 );
      String txt=type+System.getProperty("line.separator","\n"); //$NON-NLS-1$//$NON-NLS-2$
      return new CompletionProposal(txt , offset, 0, offset+txt.length(),HaskellUIImages.getImage( IImageNames.TYPE_SIGNATURE ),getLabel(),null,null );
     // doc.replace( offset, 0, type+System.getProperty("line.separator","\n") );

    } catch( BadLocationException ex ) {
      HaskellUIPlugin.log( ex );
    }
    return null;
  }

}
