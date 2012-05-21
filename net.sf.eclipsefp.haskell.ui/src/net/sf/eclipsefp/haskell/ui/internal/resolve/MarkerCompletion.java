package net.sf.eclipsefp.haskell.ui.internal.resolve;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.ui.texteditor.IDocumentProvider;

/**
 * <p>
 * Bridges the gap between IMarkerResolution and QuickAssist CompletionProposal
 * </p>
 *
 * @author JP Moresmau
 */
public abstract class MarkerCompletion implements IMarkerResolution {

  /**
   * @param marker
   *          the marker carrying the error/warning
   * @param document
   *          the document we are working in
   * @return a completion proposal
   */
  public abstract ICompletionProposal getCompletionProposal(
      final IMarker marker, final IDocument document );

  /**
   * applies the completion chosen for the given marker to the document
   */
  @Override
  public void run( final IMarker marker ) {
    try {
      IFile f = ( IFile )marker.getResource();
      IDocument doc = null;
      for( IEditorReference er: HaskellUIPlugin.getDefault().getWorkbench()
          .getActiveWorkbenchWindow().getActivePage().getEditorReferences() ) {
        IEditorInput input = er.getEditorInput();
        if( input instanceof IFileEditorInput ) {
          if( f.equals( ( ( IFileEditorInput )input ).getFile() ) ) {
            IEditorPart editor = er.getEditor( true );
            if( editor instanceof HaskellEditor ) {
              doc = ( ( HaskellEditor )editor ).getDocument();
            } else if (editor instanceof CabalFormEditor ) {
              doc = ( ( CabalFormEditor )editor ).getModel();
            }
          }
        }
      }
      IDocumentProvider prov = null;
      if( doc == null ) {
        prov = new TextFileDocumentProvider();
        prov.connect( f );
        doc = prov.getDocument( f );
      }
      ICompletionProposal cp = getCompletionProposal( marker, doc );
      if( cp != null ) {
        cp.apply( doc );
        if( prov != null ) {
          prov.saveDocument( new NullProgressMonitor(), f, doc, true );
        }
        marker.delete();
      }
    } catch( CoreException ex ) {
      HaskellUIPlugin.log( ex );
    }
  }

  protected String getLineStartAddition( final String added,
      final IResource file ) {
    if( FileUtil.hasLiterateExtension( file ) ) {
      return "> " + added;
    }
    return added;
  }

  public static class MarkerCompletionProposal implements ICompletionProposal {

    private final String fDisplayString;
    private final String fReplacementString;
    private final int fReplacementOffset;
    private final int fReplacementLength;
    private final int fCursorPosition;
    private final IMarker fMarker;
    private final String fAdditionalInfo;

    public MarkerCompletionProposal( final String replacementString,
        final int replacementOffset, final int replacementLength,
        final int cursorPosition, final String displayString,final IMarker marker,final String additionalInfo ) {
      Assert.isNotNull( replacementString );
      Assert.isTrue( replacementOffset >= 0 );
      Assert.isTrue( replacementLength >= 0 );
      Assert.isTrue( cursorPosition >= 0 );

      this.fReplacementString = replacementString;
      this.fReplacementOffset = replacementOffset;
      this.fReplacementLength = replacementLength;
      this.fCursorPosition = cursorPosition;

      this.fDisplayString = displayString;
      this.fMarker=marker;
      this.fAdditionalInfo=additionalInfo;

    }

    @Override
    public void apply( final IDocument document ) {
      try {
        document.replace( this.fReplacementOffset, this.fReplacementLength,
            this.fReplacementString );

      } catch( BadLocationException localBadLocationException ) {
        HaskellUIPlugin.log( localBadLocationException );
      }
      try {
        if (this.fMarker!=null){
          this.fMarker.delete();
        }
      } catch (CoreException ce){
        HaskellUIPlugin.log( ce );
      }
    }

    @Override
    public Point getSelection( final IDocument document ) {
      return new Point( this.fReplacementOffset + this.fCursorPosition, 0 );
    }

    @Override
    public IContextInformation getContextInformation() {
      return null;
    }

    @Override
    public Image getImage() {
      return null;
    }

    @Override
    public String getDisplayString() {
      if( this.fDisplayString != null ) {
        return this.fDisplayString;
      }
      return this.fReplacementString;
    }

    @Override
    public String getAdditionalProposalInfo() {
      return fAdditionalInfo;
    }
  }


}
