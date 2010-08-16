package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.ITextEditor;

public class WorkbenchHaskellCompletionContext extends HaskellCompletionContext {



  public WorkbenchHaskellCompletionContext( final ITextViewer viewer,
      final int offset ) {
    super( getFile( viewer ), viewer.getDocument().get(), offset );
  }

	private static IFile getFile(final ITextViewer viewer) {
		IDocument currentDocument = viewer.getDocument();

		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		IEditorReference editorReferences[] = window.getActivePage().getEditorReferences();

		IEditorInput input = null;

		for (int i = 0; i < editorReferences.length; i++) {
			IEditorPart editor = editorReferences[i].getEditor(false);
			if (editor instanceof ITextEditor) {
				ITextEditor textEditor = (ITextEditor) editor;
				IDocument doc = textEditor.getDocumentProvider().getDocument(textEditor.getEditorInput());
				if (currentDocument.equals(doc)) {
					input = textEditor.getEditorInput();
			    if (input instanceof IFileEditorInput) {
			      IFileEditorInput fileInput = (IFileEditorInput) input;
			      return fileInput.getFile();
			    }
				}
			}
		}
    // Return a null IFile, which is handled in HaskellCompletionContext.
		return null;
	}
}
