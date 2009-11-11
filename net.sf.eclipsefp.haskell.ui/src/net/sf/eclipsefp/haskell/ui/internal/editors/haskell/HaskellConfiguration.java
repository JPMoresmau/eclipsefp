// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import net.sf.eclipsefp.haskell.core.internal.contenttypes.LiterateContentDescriber;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist.HaskellCAProcessor;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.AnnotationHover;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.HaskellAutoIndentStrategy;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.HaskellCharacterScanner;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.HaskellCommentScanner;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.HaskellReconcilingStrategy;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.HaskellStringScanner;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.ScannerManager;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import net.sf.eclipsefp.haskell.ui.internal.resolve.QuickAssistProcessor;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.IAutoEditStrategy;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.quickassist.IQuickAssistAssistant;
import org.eclipse.jface.text.quickassist.QuickAssistAssistant;
import org.eclipse.jface.text.reconciler.IReconciler;
import org.eclipse.jface.text.reconciler.IReconcilingStrategy;
import org.eclipse.jface.text.reconciler.MonoReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.source.IAnnotationHover;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.ui.IFileEditorInput;

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

	final HaskellEditor editor;
	private final IContentAssistantFactory fFactory;

	public HaskellConfiguration( final HaskellEditor editor ) {
    this( editor, new ContentAssistantFactory() );
  }

  public HaskellConfiguration(
      final HaskellEditor editor,
      final IContentAssistantFactory factory ) {
    this.editor = editor;
    fFactory = factory;
  }


	// interface methods of SourceViewerConfiguration
	// ///////////////////////////////////////////////

  @Override
  public ITextHover getTextHover( final ISourceViewer sourceViewer,
                                  final String contentType ) {
    ITextHover result = null;
    if( IDocument.DEFAULT_CONTENT_TYPE.equals( contentType ) ) {
      result = new HaskellTextHover( editor, sourceViewer );
    }
    return result;
  }

	@Override
  public IAutoEditStrategy[] getAutoEditStrategies(
      final ISourceViewer sv, final String contentType ) {
    return new IAutoEditStrategy[] { new HaskellAutoIndentStrategy() };
  }

	@Override
  public String[] getConfiguredContentTypes( final ISourceViewer sv ) {
    return new String[] { IDocument.DEFAULT_CONTENT_TYPE, // plain text
      IPartitionTypes.HS_LITERATE_COMMENT,
      IPartitionTypes.HS_COMMENT,
      IPartitionTypes.HS_CHARACTER,
      IPartitionTypes.HS_STRING
    };
  }

	@Override
  public int getTabWidth( final ISourceViewer sourceViewer ) {
    return getPreferenceStore().getInt( EDITOR_TAB_WIDTH );
  }

	@Override
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
	@Override
  public IPresentationReconciler getPresentationReconciler(
			final ISourceViewer sv) {
		PresentationReconciler reconciler = new PresentationReconciler();

		// for every content type we need a damager and a repairer:
		ScannerManager man = ScannerManager.getInstance();
    ITokenScanner codeScanner = man.getCodeScanner( isLatexLiterate() );
    DefaultDamagerRepairer dr = new DefaultDamagerRepairer( codeScanner );
    reconciler.setDamager( dr, IDocument.DEFAULT_CONTENT_TYPE );
    reconciler.setRepairer( dr, IDocument.DEFAULT_CONTENT_TYPE );
    // comments
    HaskellCommentScanner commentScanner = man.getCommentScanner();
    DefaultDamagerRepairer cndr = new DefaultDamagerRepairer( commentScanner );
    reconciler.setDamager( cndr, IPartitionTypes.HS_COMMENT );
    reconciler.setRepairer( cndr, IPartitionTypes.HS_COMMENT );
    // string literals
    HaskellStringScanner stringScanner = man.getStringScanner();
    DefaultDamagerRepairer sndr = new DefaultDamagerRepairer( stringScanner );
    reconciler.setDamager( sndr, IPartitionTypes.HS_STRING );
    reconciler.setRepairer( sndr, IPartitionTypes.HS_STRING );
    // character literals
    HaskellCharacterScanner charScanner = man.getCharacterScanner();
    DefaultDamagerRepairer chndr = new DefaultDamagerRepairer( charScanner );
    reconciler.setDamager( chndr, IPartitionTypes.HS_CHARACTER );
    reconciler.setRepairer( chndr, IPartitionTypes.HS_CHARACTER );
    // literate comments
    HaskellCommentScanner litScanner = man.getLiterateCommentScanner();
    DefaultDamagerRepairer lcndr = new DefaultDamagerRepairer( litScanner );
    reconciler.setDamager( lcndr, IPartitionTypes.HS_LITERATE_COMMENT );
    reconciler.setRepairer( lcndr, IPartitionTypes.HS_LITERATE_COMMENT );

		return reconciler;
	}

  @Override
  public IAnnotationHover getAnnotationHover(final ISourceViewer sv) {
		return new AnnotationHover();
	}

	@Override
  public String[] getDefaultPrefixes(final ISourceViewer sourceViewer,
			final String contentType) {

		return new String[] { "--" }; //$NON-NLS-1$
	}

	@Override
  public String[] getIndentPrefixes(final ISourceViewer sourceViewer,
			final String contentType) {

		int tabWidth = getTabWidth(sourceViewer);
		StringBuilder prefix = new StringBuilder();
		String[] ret=new String[tabWidth+2];
		for (int i = 0; i <= tabWidth; i++) {

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
			ret[i]=prefix.toString();
			prefix.setLength( 0 );
		}
		ret[tabWidth+1]=""; //$NON-NLS-1$
		return ret;
	}

	@Override
	public IQuickAssistAssistant getQuickAssistAssistant(
	    final ISourceViewer sourceViewer ) {
    QuickAssistAssistant qaa=new QuickAssistAssistant();
    qaa.setQuickAssistProcessor( new QuickAssistProcessor() );
    return qaa;

	}

	@Override
  public IReconciler getReconciler( final ISourceViewer sourceViewer ) {
    MonoReconciler result = null;
    // the editor may be null if this configuration is used in a preview
    // (source viewer without editor)
    if( editor != null ) {
      IReconcilingStrategy strategy = new HaskellReconcilingStrategy( editor );
      result = new MonoReconciler( strategy, false );
      result.setProgressMonitor( new NullProgressMonitor() );
      result.setDelay( 500 );
    }
    return result;
  }

	// helping methods
	// ////////////////

	private IPreferenceStore getPreferenceStore() {
		return HaskellUIPlugin.getDefault().getPreferenceStore();
	}

	private boolean isSpacesForTabs() {
    String key = IEditorPreferenceNames.EDITOR_SPACES_FOR_TABS;
    return getPreferenceStore().getBoolean( key );
  }

  private boolean isLatexLiterate() {
    boolean result = false;
    if( editor != null && editor.getEditorInput() instanceof IFileEditorInput ) {
      IFileEditorInput fei = ( IFileEditorInput )editor.getEditorInput();
      IFile file = fei.getFile();
      try {
        if( file != null && file.getContentDescription() != null ) {
          QualifiedName name = LiterateContentDescriber.STYLE;
          Object property = file.getContentDescription().getProperty( name );
          result = LiterateContentDescriber.LATEX.equals( property );
        }
      } catch( final CoreException cex ) {
        HaskellUIPlugin.log( cex );
      }
    }
    return result;
  }
}