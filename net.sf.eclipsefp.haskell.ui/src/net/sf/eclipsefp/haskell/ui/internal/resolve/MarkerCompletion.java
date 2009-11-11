package net.sf.eclipsefp.haskell.ui.internal.resolve;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.ui.texteditor.IDocumentProvider;

/**
 * <p>Bridges the gap between IMarkerResolution and QuickAssist CompletionProposal</p>
  *
  * @author JP Moresmau
 */
public abstract class MarkerCompletion implements IMarkerResolution {

  /**
   * @param marker the marker carrying the error/warning
   * @param document the document we are working in
   * @return a completion proposal
   */
  public abstract ICompletionProposal getCompletionProposal( final IMarker marker,final IDocument document);

  public void run( final IMarker marker ) {
    try {
      IFile f=(IFile)marker.getResource();
      IDocument doc=null;
      for (IEditorReference er:HaskellUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage().getEditorReferences()){
        IEditorInput input = er.getEditorInput();
        if( input instanceof IFileEditorInput) {
          if (f.equals( (( IFileEditorInput )input ).getFile())){
            IEditorPart editor=er.getEditor( true );
            if (editor instanceof HaskellEditor){
              doc=((HaskellEditor)editor).getDocument();
            }
          }
        }
      }
      IDocumentProvider prov=null;
      if (doc==null){
        prov=new TextFileDocumentProvider();
        prov.connect( f );
        doc=prov.getDocument( f );
      }
      ICompletionProposal cp=getCompletionProposal( marker,doc );
      if (cp!=null){
        cp.apply( doc );
        if(prov!=null){
          prov.saveDocument( new NullProgressMonitor(), f, doc, true );
        }
      }
    } catch( CoreException ex ) {
      HaskellUIPlugin.log( ex );
    }
  }
}
