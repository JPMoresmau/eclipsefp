// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal;

import java.util.Iterator;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.editor.actions.IEditorActionDefinitionIds;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.outline.CabalOutlinePage;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.QuickFixAction;
import net.sf.eclipsefp.haskell.ui.internal.editors.text.HaskellViewer;
import net.sf.eclipsefp.haskell.ui.internal.resolve.SelectAnnotationForQuickFix;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.jface.text.source.IVerticalRulerInfo;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.jface.text.source.projection.ProjectionSupport;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.eclipse.ui.texteditor.MarkerAnnotation;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

/** <p>an editor for Cabal package description files.</p>
  *
  * <p>Note: this class is declared in <code>plugin.xml</code>.</p>
  *
  * @author Leif Frenzel
  */
public class CabalEditor extends TextEditor {

  private CabalOutlinePage outlinePage;
  private ProjectionSupport projectionSupport;
  private final CabalFormEditor formEditor;

  /** The key binding context active while the Cabal editor is active */
  private static final String CONTEXT_ID = CabalEditor.class.getName() + ".context";  //$NON-NLS-1$

  public CabalEditor(final CabalFormEditor formEditor) {
    this.formEditor=formEditor;
    setSourceViewerConfiguration( new CabalConfiguration( formEditor ) );
  }

  IDocument getDocument() {
    IDocument result = null;
    if( getSourceViewer() != null ) {
      result = getSourceViewer().getDocument();
    }
    return result;
  }

  public void setPackageDescription( final PackageDescription packageDescription ) {
    if( outlinePage != null ) {
      outlinePage.setPackageDescription( packageDescription );
    }
  }

  public void selectAndReveal( final Object element ) {
    if( element instanceof PackageDescriptionStanza ) {
      PackageDescriptionStanza stanza = (PackageDescriptionStanza) element;
      IDocument doc = getSourceViewer().getDocument();
      try {
        int offset = doc.getLineOffset( stanza.getStartLine() );
        int end = doc.getLength();
        try {
          end =   doc.getLineOffset( stanza.getEndLine()-1 )
                + doc.getLineLength( stanza.getEndLine()-1 );
        } catch( final BadLocationException badlox ) {
          // ignore
        }
        int length = end - offset;
        selectAndReveal( offset, length );
      } catch( final BadLocationException badlox ) {
        // ignore
      }
    }
  }

  // interface methods of TextEditor
  //////////////////////////////////

  @Override
  protected ISourceViewer createSourceViewer( final Composite parent,
                                              final IVerticalRuler ruler,
                                              final int styles ) {
    fOverviewRuler = createOverviewRuler( getSharedColors() );
    ISourceViewer viewer
      = new HaskellViewer( parent, ruler, fOverviewRuler, true, styles );
    // ensure decoration support has been created and configured:
    getSourceViewerDecorationSupport( viewer );
    return viewer;
  }

  @Override
  public void createPartControl( final Composite parent ) {
    super.createPartControl( parent );

    ProjectionViewer pv = ( ProjectionViewer )getSourceViewer();
    projectionSupport = new ProjectionSupport( pv,
                                               getAnnotationAccess(),
                                               getSharedColors() );
    projectionSupport.install();
    projectionSupport.addSummarizableAnnotationType("org.eclipse.ui.workbench.texteditor.error"); //$NON-NLS-1$
    projectionSupport.addSummarizableAnnotationType("org.eclipse.ui.workbench.texteditor.warning"); //$NON-NLS-1$
    pv.doOperation( ProjectionViewer.TOGGLE );
    activateContext();
  }

  private void activateContext() {
    IContextService contextService = ( IContextService )getSite().getService( IContextService.class );
    contextService.activateContext( CONTEXT_ID );
  }


  @Override
  protected void createActions() {
    super.createActions();
    HaskellViewer.createTextOpAction(this,  "ContentAssist.", "ContentAssistProposal", ISourceViewer.CONTENTASSIST_PROPOSALS,
        ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS );

    // comment/uncomment
    HaskellViewer.createTextOpAction(this, HaskellEditor.LINE_COMMENT_ACTION, HaskellEditor.commentResourcePrefix, HaskellViewer.TOGGLE_COMMENT,
                        IEditorActionDefinitionIds.COMMENT );
//    HaskellEditor.createTextOpAction(this, HaskellEditor.LINE_UNCOMMENT_ACTION, HaskellEditor.uncommentResourcePrefix, ITextOperationTarget.STRIP_PREFIX,
//                        IEditorActionDefinitionIds.UNCOMMENT );

    addRulerContextMenuListener( new IMenuListener() {

      @Override
      public void menuAboutToShow( final IMenuManager manager ) {
       IVerticalRulerInfo service= (IVerticalRulerInfo)getAdapter(IVerticalRulerInfo.class);
       if (service!=null){
         int line=service.getLineOfLastMouseButtonActivity();

         for (Iterator<?> it=getSourceViewer().getAnnotationModel().getAnnotationIterator();it.hasNext();){
           Annotation ann = (Annotation) it.next();
           if (ann instanceof MarkerAnnotation){
             Position p=getSourceViewer().getAnnotationModel().getPosition( ann );
             try {
             if (p!=null && getDocument().getLineOfOffset(p.getOffset())==line){
               if (((ProjectionViewer)getSourceViewer()).getQuickAssistAssistant().canFix( ann )){
                 IAction action=new SelectAnnotationForQuickFix(  CabalEditor.this , (SourceViewer)getSourceViewer(), (MarkerAnnotation)ann );
                 manager.add( action );
                 //return;
               }
             }
             } catch (BadLocationException ble){
               // ignore
             }
           }
         }
       }
      }
    });
    QuickFixAction action = new QuickFixAction(HaskellUIPlugin.getDefault().getResourceBundle(), "RulerQuickFixAction", this, getVerticalRuler()); //$NON-NLS-1$
    setAction(ITextEditorActionConstants.RULER_CLICK, action);

  }

  @Override
  protected void initializeEditor() {
    super.initializeEditor();
  }

  @Override
  public void editorContextMenuAboutToShow( final IMenuManager menu ) {
    super.editorContextMenuAboutToShow( menu );
    if( isEditable() ) {
      IMenuManager mmSource = new MenuManager( UITexts.editor_actions_source, "source" ); //$NON-NLS-1$
      menu.prependToGroup( ITextEditorActionConstants.GROUP_EDIT, mmSource );

      mmSource.add( new Separator( "comments" ) ); //$NON-NLS-1$

      addAction( mmSource, "comments", HaskellEditor.LINE_COMMENT_ACTION ); //$NON-NLS-1$
      addAction( mmSource, "comments", HaskellEditor.LINE_UNCOMMENT_ACTION ); //$NON-NLS-1$

    }
  }

  // interface methods of IAdaptable
  //////////////////////////////////

  @Override
  public Object getAdapter(
      @SuppressWarnings("rawtypes") final Class adapterType ) {
    Object result = null;
    if( IContentOutlinePage.class.equals( adapterType ) ) {
      if( outlinePage == null ) {
        outlinePage = new CabalOutlinePage( this, formEditor.getPackageDescription() );
      }
      result = outlinePage;
    } else if( projectionSupport != null ) {
      Object adapter
        = projectionSupport.getAdapter( getSourceViewer(), adapterType );
      if( adapter != null ) {
        result = adapter;
      }
    }

    if( result == null ) {
      result = super.getAdapter( adapterType );
    }
    return result;
  }
}
