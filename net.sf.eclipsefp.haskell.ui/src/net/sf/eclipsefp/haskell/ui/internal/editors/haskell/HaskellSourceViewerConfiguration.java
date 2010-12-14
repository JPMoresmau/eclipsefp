// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist.HaskellContentAssistProcessor;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.AnnotationHover;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.HaskellAutoIndentStrategy;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.HaskellReconcilingStrategy;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.ScannerManager;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.ScionTokenScanner;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.SyntaxPreviewer;
import net.sf.eclipsefp.haskell.ui.internal.resolve.QuickAssistProcessor;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.NullProgressMonitor;
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

/**
 * The source viewer configuration implements functionality for the Haskell editor.
 *
 * @author Leif Frenzel
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public class HaskellSourceViewerConfiguration extends SourceViewerConfiguration implements IEditorPreferenceNames {
  /** The associated Haskell editor */
	final HaskellEditor editor;
	/** The plugin's preference store, needed to create a new ScannerManager */
	private IPreferenceStore prefStore;
	/** The syntax highlighting and other content management container */
	private ScannerManager scannerManager;

	/** The constructor
	 *
	 * @param editor The associated Haskell editor
	 */
	public HaskellSourceViewerConfiguration( final HaskellEditor editor ) {
    this.editor = editor;
  }

	/** Set the source viewer's preference store.
   *
   * <p>Note: Currently only referenced by {@link SyntaxPreviewer}</p>
	 *
	 * @param prefStore The preference store
	 */
  public void setPreferenceStore( final IPreferenceStore prefStore ) {
    this.prefStore = prefStore;
  }

	// interface methods of SourceViewerConfiguration
	// ///////////////////////////////////////////////

  @Override
  public ITextHover getTextHover( final ISourceViewer sourceViewer, final String contentType ) {
    ITextHover result = null;
    if( IDocument.DEFAULT_CONTENT_TYPE.equals( contentType ) ) {
      result = new HaskellTextHover( editor, sourceViewer );
    }
    return result;
  }

	@Override
  public IAutoEditStrategy[] getAutoEditStrategies( final ISourceViewer sv, final String contentType ) {
    return new IAutoEditStrategy[] { new HaskellAutoIndentStrategy() };
  }

	@Override
  public String[] getConfiguredContentTypes( final ISourceViewer sv ) {
    return new String[] { IDocument.DEFAULT_CONTENT_TYPE // plain text
//      IPartitionTypes.HS_LITERATE_COMMENT,
//      IPartitionTypes.HS_COMMENT,
//      IPartitionTypes.HS_CHARACTER,
//      IPartitionTypes.HS_STRING
    };
  }

	@Override
  public int getTabWidth( final ISourceViewer sourceViewer ) {
    return getPreferenceStore().getInt( EDITOR_TAB_WIDTH );
  }

	@Override
  public IContentAssistant getContentAssistant(final ISourceViewer viewer) {

		ContentAssistant result = new ContentAssistant();
		result.setContentAssistProcessor(new HaskellContentAssistProcessor(), IDocument.DEFAULT_CONTENT_TYPE);
		result.setProposalPopupOrientation(IContentAssistant.PROPOSAL_OVERLAY);

		// TODO get from pref / update on pref change
		result.enableAutoActivation(true);
		result.enableAutoInsert(true);
		result.setAutoActivationDelay(500);

		return result;
	}

	/**
	 * Get the scanner manager. If the preference store (prefStore) is set, then return a new {@link ScannerManager}
	 * that uses the preference store; otherwise, return the ScannerManager singleton instance.
	 * */
	public ScannerManager getScannerManager() {
	    if ( prefStore != null ) {
	        if ( scannerManager == null ){
	          scannerManager = new ScannerManager( prefStore );
	        }
	        return scannerManager;
	    }
	    return ScannerManager.getInstance();
	}

	/** the presentation reconciler is responsible for syntax coloring. */
	@Override
  public IPresentationReconciler getPresentationReconciler(final ISourceViewer sv) {
		PresentationReconciler reconciler = new PresentationReconciler();

		// for every content type we need a damager and a repairer:
    //		ScannerManager man = getScannerManager();
    // ITokenScanner codeScanner = man.getCodeScanner( isLatexLiterate() );
		ScionInstance instance=null;
		if ( editor != null ){
		  // Get the shared scion-server instance for lexing.
		  instance = ScionPlugin.getSharedScionInstance();
		  Assert.isNotNull( instance );
		} // else no editor: we're in preview null instance is fine


		IFile file=editor!=null?editor.findFile():null;
		ITokenScanner codeScanner=new ScionTokenScanner(getScannerManager(),instance, file);
    DefaultDamagerRepairer dr = new DefaultDamagerRepairer( codeScanner );
    reconciler.setDamager( dr, IDocument.DEFAULT_CONTENT_TYPE );
    reconciler.setRepairer( dr, IDocument.DEFAULT_CONTENT_TYPE );
    // comments
    //HaskellCommentScanner commentScanner = man.getCommentScanner();
    //DefaultDamagerRepairer cndr = new DefaultDamagerRepairer( codeScanner );
//    reconciler.setDamager( dr, IPartitionTypes.HS_COMMENT );
//    reconciler.setRepairer( dr, IPartitionTypes.HS_COMMENT );
//    // string literals
//    //HaskellStringScanner stringScanner = man.getStringScanner();
//    //DefaultDamagerRepairer sndr = new DefaultDamagerRepairer( codeScanner );
//    reconciler.setDamager( dr, IPartitionTypes.HS_STRING );
//   reconciler.setRepairer( dr, IPartitionTypes.HS_STRING );
//    // character literals
//    //HaskellCharacterScanner charScanner = man.getCharacterScanner();
//    //DefaultDamagerRepairer chndr = new DefaultDamagerRepairer( codeScanner );
//    reconciler.setDamager( dr, IPartitionTypes.HS_CHARACTER );
//    reconciler.setRepairer( dr, IPartitionTypes.HS_CHARACTER );
//    // literate comments
//    //HaskellCommentScanner litScanner = man.getLiterateCommentScanner();
//    //DefaultDamagerRepairer lcndr = new DefaultDamagerRepairer( codeScanner );
//    reconciler.setDamager( dr, IPartitionTypes.HS_LITERATE_COMMENT );
//    reconciler.setRepairer( dr, IPartitionTypes.HS_LITERATE_COMMENT );

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
	  if (prefStore!=null){
	    return prefStore;
	  }
		return HaskellUIPlugin.getDefault().getPreferenceStore();
	}

	private boolean isSpacesForTabs() {
    String key = IEditorPreferenceNames.EDITOR_SPACES_FOR_TABS;
    return getPreferenceStore().getBoolean( key );
  }

//  private boolean isLatexLiterate() {
//    boolean result = false;
//    if( editor != null && editor.getEditorInput() instanceof IFileEditorInput ) {
//      IFileEditorInput fei = ( IFileEditorInput )editor.getEditorInput();
//      IFile file = fei.getFile();
//      try {
//        if( file != null && file.getContentDescription() != null ) {
//          QualifiedName name = LiterateContentDescriber.STYLE;
//          Object property = file.getContentDescription().getProperty( name );
//          result = LiterateContentDescriber.LATEX.equals( property );
//        }
//      } catch( final CoreException cex ) {
//        HaskellUIPlugin.log( cex );
//      }
//    }
//    return result;
//  }
}