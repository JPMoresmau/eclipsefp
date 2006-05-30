// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.editor;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.IAutoIndentStrategy;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.reconciler.*;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.source.*;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.editor.codeassist.HaskellCAProcessor;
import net.sf.eclipsefp.haskell.ui.editor.text.*;
import net.sf.eclipsefp.haskell.ui.preferences.editor.IEditorPreferenceNames;

/**
 * <p>
 * The source viewer configuration provides strategies for functionality of the
 * Haskell editor.
 * </p>
 * 
 * @author Leif Frenzel
 */
public class HaskellConfiguration extends SourceViewerConfiguration implements
		IEditorPreferenceNames {

	public static class ContentAssistantFactory implements IContentAssistantFactory {

		public ContentAssistant createAssistant() {
			return new ContentAssistant();
		}

	}

	private final HaskellEditor fEditor;
	private IContentAssistantFactory fFactory;

	public HaskellConfiguration(final HaskellEditor editor) {
		this(editor, new ContentAssistantFactory());
	}

	public HaskellConfiguration(final HaskellEditor editor, IContentAssistantFactory factory) {
		fEditor = editor;
		fFactory = factory;
	}

	// interface methods of SourceViewerConfiguration
	// ///////////////////////////////////////////////

	public IAutoIndentStrategy getAutoIndentStrategy(final ISourceViewer sv,
			final String contentType) {
		return new HaskellAutoIndentStrategy();
	}

	public String[] getConfiguredContentTypes(final ISourceViewer sv) {
		return new String[] { IDocument.DEFAULT_CONTENT_TYPE, // plain text
				IPartitionTypes.HS_LITERATE_COMMENT, IPartitionTypes.HS_COMMENT };
	}

	public int getTabWidth(final ISourceViewer sourceViewer) {
		return getPreferenceStore().getInt(EDITOR_TAB_WIDTH);
	}

	public IContentAssistant getContentAssistant(final ISourceViewer viewer) {

		ContentAssistant result = fFactory.createAssistant();
		result.setContentAssistProcessor(new HaskellCAProcessor(),
				IDocument.DEFAULT_CONTENT_TYPE);
		result.setProposalPopupOrientation(IContentAssistant.PROPOSAL_OVERLAY);

		// TODO get from pref / update on pref change
		result.enableAutoActivation(true);
		result.enableAutoInsert(true);
		result.setAutoActivationDelay(500);

		return result;
	}

	/** the presentation reconciler is responsible for syntax coloring. */
	public IPresentationReconciler getPresentationReconciler(
			final ISourceViewer sv) {
		PresentationReconciler reconciler = new PresentationReconciler();

		// for every content type we need a damager and a repairer:
		// plain text between tags
		ScannerManager man = ScannerManager.getInstance();
		ITokenScanner codeScanner = man.getCodeScanner();
		DefaultDamagerRepairer dr = new DefaultDamagerRepairer(codeScanner);
		reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
		reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);
		// comments
		HaskellCommentScanner commentScanner = man.getCommentScanner();
		DefaultDamagerRepairer cndr = new DefaultDamagerRepairer(commentScanner);
		reconciler.setDamager(cndr, IPartitionTypes.HS_COMMENT);
		reconciler.setRepairer(cndr, IPartitionTypes.HS_COMMENT);
		// literate comments
		HaskellCommentScanner litScanner = man.getLiterateCommentScanner();
		DefaultDamagerRepairer lcndr = new DefaultDamagerRepairer(litScanner);
		reconciler.setDamager(lcndr, IPartitionTypes.HS_LITERATE_COMMENT);
		reconciler.setRepairer(lcndr, IPartitionTypes.HS_LITERATE_COMMENT);

		return reconciler;
	}

	public IAnnotationHover getAnnotationHover(final ISourceViewer sv) {
		return new AnnotationHover();
	}

	public String[] getDefaultPrefixes(final ISourceViewer sourceViewer,
			final String contentType) {

		return new String[] { "--" };
	}

	public String[] getIndentPrefixes(final ISourceViewer sourceViewer,
			final String contentType) {
		List list = new ArrayList();
		int tabWidth = getTabWidth(sourceViewer);
		for (int i = 0; i <= tabWidth; i++) {
			StringBuffer prefix = new StringBuffer();
			if (isSpacesForTabs()) {
				for (int j = 0; j + i < tabWidth; j++) {
					prefix.append(' ');
				}
				if (i != 0) {
					prefix.append('\t');
				}
			} else {
				for (int j = 0; j < i; j++) {
					prefix.append(' ');
				}
				if (i != tabWidth) {
					prefix.append('\t');
				}
			}
			list.add(prefix.toString());
		}
		list.add("");

		String[] result = new String[list.size()];
		list.toArray(result);
		return result;
	}

	public IReconciler getReconciler(final ISourceViewer sourceViewer) {
		MonoReconciler result = null;
		// the editor may be null if this configuration is used in a preview
		// (source viewer without editor)
		if (fEditor != null) {
			IReconcilingStrategy strategy = new HaskellReconcilingStrategy(
					fEditor);
			result = new MonoReconciler(strategy, false);
			result.setProgressMonitor(new NullProgressMonitor());
			result.setDelay(500);
		}
		return result;
	}

	// helping methods
	// ////////////////

	private IPreferenceStore getPreferenceStore() {
		return HaskellUIPlugin.getDefault().getPreferenceStore();
	}

	// experimental from here

	private boolean isSpacesForTabs() {
		String key = IEditorPreferenceNames.EDITOR_SPACES_FOR_TABS;
		return getPreferenceStore().getBoolean(key);
	}
}