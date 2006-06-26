// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.editor.text;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.reconciler.DirtyRegion;
import org.eclipse.jface.text.reconciler.IReconcilingStrategy;
import org.eclipse.jface.text.reconciler.IReconcilingStrategyExtension;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;

import net.sf.eclipsefp.haskell.core.halamo.ICompilationUnit;
import net.sf.eclipsefp.haskell.core.parser.IHaskellParser;
import net.sf.eclipsefp.haskell.core.parser.ParserManager;
import net.sf.eclipsefp.haskell.ui.editor.HaskellEditor;

/**
 * <p>
 * helper class that defines the model reconciling for the Haskell editor.
 * </p>
 * 
 * @author Leif Frenzel
 */
public class HaskellReconcilingStrategy implements IReconcilingStrategy,
		IReconcilingStrategyExtension {

	private final HaskellEditor editor;

	private final HaskellFoldingStructureProvider foldingStructureProvider;

	public HaskellReconcilingStrategy(final HaskellEditor editor) {
		this.editor = editor;
		foldingStructureProvider = new HaskellFoldingStructureProvider(editor);
	}

	// interface methods of IReconcilingStrategy
	// //////////////////////////////////////////

	public void setDocument(final IDocument document) {
		foldingStructureProvider.setDocument(document);
	}

	public void reconcile(final DirtyRegion dirtyRegion, final IRegion subRegion) {
		reconcile();
	}

	public void reconcile(final IRegion partition) {
		reconcile();
	}

	// interface methods of IReconcilingStrategyExtension
	// ///////////////////////////////////////////////////

	public void setProgressMonitor(final IProgressMonitor monitor) {
		foldingStructureProvider.setProgressMonitor(monitor);
	}

	public void initialReconcile() {
		reconcile();
	}

	// helping methods
	// ////////////////

	private void reconcile() {
		IEditorInput input = editor.getEditorInput();
		if (input != null && input instanceof IFileEditorInput) {
			IFile file = ((IFileEditorInput) input).getFile();

			//TODO this is here for when the reconciler gets called but the
			//file has already been deleted
			//check to see if we can use a PlatformJob or something to run
			//this in safe state
			if (!file.exists())
				return;

			final ICompilationUnit cu = parse(file);
			if (cu != null) {
				Shell shell = editor.getSite().getShell();
				if (shell != null && !shell.isDisposed()) {
					shell.getDisplay().asyncExec(new Runnable() {
						public void run() {
							editor.setModel(cu);
						}
					});
					foldingStructureProvider.updateFoldingRegions(cu);
				}
			}
		}
	}

	private ICompilationUnit parse(final IFile file) {
		ICompilationUnit result = null;
		ParserManager manager = ParserManager.getInstance();
		IHaskellParser parser = manager.getParser();
		if (parser != null && parser.canParse()) {
			try {
				result = parser.parse(file);
			} catch (final CoreException cex) {
				// TODO what error handling here?
				cex.printStackTrace();
			}
		}
		return result;
	}
}
