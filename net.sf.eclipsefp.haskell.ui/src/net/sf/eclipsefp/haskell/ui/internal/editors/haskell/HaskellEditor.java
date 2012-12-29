/**
 *  Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
 * (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 *
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;
import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.JobFacade;
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.buildwrapper.types.NameDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.NameDefHandler;
import net.sf.eclipsefp.haskell.buildwrapper.types.Note;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineDef.OutlineDefType;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineHandler;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineResult;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.editor.actions.IEditorActionDefinitionIds;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.actions.FormatAction;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.actions.HaddockBlockDocumentFollowingAction;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.actions.HaddockDocumentFollowingAction;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.actions.HaddockDocumentPreviousAction;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.actions.PragmaCommentAction;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports.ImportsManager;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.HaskellCharacterPairMatcher;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.HaskellFoldingStructureProvider;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.ScionTokenScanner;
import net.sf.eclipsefp.haskell.ui.internal.editors.text.MarkOccurrenceComputer;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import net.sf.eclipsefp.haskell.ui.internal.resolve.SelectAnnotationForQuickFix;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.internal.views.outline.HaskellOutlinePage;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.jface.text.source.IVerticalRulerInfo;
import org.eclipse.jface.text.source.projection.ProjectionSupport;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.progress.UIJob;
import org.eclipse.ui.texteditor.ChainedPreferenceStore;
import org.eclipse.ui.texteditor.GotoAnnotationAction;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.eclipse.ui.texteditor.MarkerAnnotation;
import org.eclipse.ui.texteditor.SourceViewerDecorationSupport;
import org.eclipse.ui.texteditor.TextOperationAction;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

/**
 * The main editor class for the Haskell editor.
 *
 * @author Leif Frenzel
 */
public class HaskellEditor extends TextEditor implements IEditorPreferenceNames, NameDefHandler {
  /** The Haskell editor's identifier. */
  public static final String ID = HaskellEditor.class.getName();
  /** Action string associated with a following Haddock documentation comment */
  public static final String HADDOCK_DOCUMENT_FOLLOWING_ACTION = "Haddock.Follow"; //$NON-NLS-1$
  /** Action string associated with a previous Haddock documentation comment */
  public static final String HADDOCK_DOCUMENT_PREVIOUS_ACTION = "Haddock.Previous"; //$NON-NLS-1$
  /** Action string associated with a following Haddock documentation comment */
  public static final String HADDOCK_BLOCK_DOCUMENT_FOLLOWING_ACTION = "Haddock.Block.Follow"; //$NON-NLS-1$
  /** Action string associated with line comment insertion */
  public static final String LINE_COMMENT_ACTION = "Comment"; //$NON-NLS-1$
  /** Action string associated with line uncommenting */
  public static final String LINE_UNCOMMENT_ACTION = "Uncomment"; //$NON-NLS-1$
  /** Action string associated with pragma comments insertion */
  public static final String COMMENT_PRAGMA_ACTION = "Comment.Pragma"; //$NON-NLS-1$
  /** Action string associated with formatting */
  public static final String FORMAT_ACTION = "Format"; //$NON-NLS-1$

  /** Resource prefix used to query properties for line comments (see plugin.properties) */
  public static final String commentResourcePrefix = "CommentAction"; //$NON-NLS-1$
  /** Resource prefix used to query properties for line commenting */
  public static final String uncommentResourcePrefix = "UncommentAction";
  /** Resource prefix used to query properties for pragma comments */
  private static final String commentPragmaResourcePrefix = "CommentPragmaAction"; //$NON-NLS-1$
  /** Resource prefix used to query properties for format */
  private static final String formatResourcePrefix = "FormatAction"; //$NON-NLS-1$

  /** The key binding context active while the Haskell editor is active */
  private static final String CONTEXT_ID = HaskellEditor.class.getName() + ".context";  //$NON-NLS-1$
  private static final String SIMPLE_CONTEXT_ID = HaskellEditor.class.getSimpleName() + "Context";

  private HaskellOutlinePage outlinePage;
  private ProjectionSupport projectionSupport;
  private MarkOccurrenceComputer markOccurrencesComputer;
  private HaskellFoldingStructureProvider foldingStructureProvider;

  //private List<OutlineDef> outline;
  private Map<String,List<OutlineDef>> defByName;

  private ScionTokenScanner tokenScanner;

  private ImportsManager importsManager;

  /**
   * the names in scope as given by GHC
   */
  private final Collection<NameDef> names=new ArrayList<NameDef>();

  /**
   * The scion-server supporting this editor.
   *
   * @note This variable isn't actually used to communicate with the
   *       scion-server. It's sole purpose is change detection, since the editor
   *       can be reused between different projects.
   */
  //private ScionInstance instance = null;

  private OutlineResult lastOutlineResult=null;
  private final OutlineHandler outlineHandler = new OutlineHandler() {

    @Override
    public void handleOutline( final OutlineResult or ) {
      if (!or.getOutlineDefs().isEmpty() || or.isBuildOK()){// avoid removing all outline on error
        if (outlinePage!=null){
          outlinePage.setInput( or.getOutlineDefs() );
        }
        lastOutlineResult=or;
        if (foldingStructureProvider!=null){
          foldingStructureProvider.updateFoldingRegions( or.getOutlineDefs() );
        }
      } else if (!or.isBuildOK() && or.getNotes()!=null && or.getNotes().size()>0 &&
           (lastOutlineResult==null || lastOutlineResult.isEmpty())){
        List<OutlineDef> errorsOutline=new ArrayList<OutlineDef>();
        for (Note n:or.getNotes()){
          if (n.getKind().equals( Note.Kind.ERROR )){
            OutlineDef def=new OutlineDef(n.getMessage(),OutlineDefType.ERROR,n.getLocation()) ;
            errorsOutline.add( def );
          }
        }
        if (outlinePage!=null){
          outlinePage.setInput( errorsOutline);
        }
        lastOutlineResult=or;
        if (foldingStructureProvider!=null){
          foldingStructureProvider.updateFoldingRegions( Collections.<OutlineDef>emptyList() );
        }
      }
    }
  };

  /** Default constructor */
  public HaskellEditor() {
    super();
  }

//  public void reveal( final IHaskellLanguageElement element ) {
//    Assert.isNotNull( element );
//    IDocument doc = getSourceViewer().getDocument();
//    ISourceLocation srcLoc = element.getSourceLocation();
//    int offset = -1;
//    try {
//      offset = doc.getLineOffset( srcLoc.getLine() ) + srcLoc.getColumn();
//    } catch( final BadLocationException badlox ) {
//      // ignore
//    }
//    int length = element.getName().length();
//    getSourceViewer().revealRange( offset, length );
//  }



  public IDocument getDocument() {

    IDocumentProvider docProvider = getDocumentProvider();
    if (docProvider!=null){
      return docProvider.getDocument( getEditorInput() );
    }
    return null;
  }

  // interface methods of TextEditor
  // ////////////////////////////////

  @Override
  protected void initializeEditor() {
    super.initializeEditor();
    setSourceViewerConfiguration( new HaskellSourceViewerConfiguration( this ) );
    setEditorContextMenuId( "#" + SIMPLE_CONTEXT_ID );  //$NON-NLS-1$
    // we configure the preferences ourselves
    IPreferenceStore generalTextStore= EditorsUI.getPreferenceStore();
    IPreferenceStore combinedPreferenceStore= new ChainedPreferenceStore(new IPreferenceStore[] {generalTextStore, HaskellUIPlugin.getDefault().getPreferenceStore() });
    setPreferenceStore( combinedPreferenceStore);
    initMarkOccurrences();
    foldingStructureProvider = new HaskellFoldingStructureProvider( this );
  }


  public HaskellFoldingStructureProvider getFoldingStructureProvider() {
    return foldingStructureProvider;
  }

  @Override
  protected boolean affectsTextPresentation( final PropertyChangeEvent evt ) {
    String prop = evt.getProperty();
    return super.affectsTextPresentation( evt ) || isAffectingProperty( prop );
  }

  @Override
  protected void configureSourceViewerDecorationSupport( final SourceViewerDecorationSupport support ) {
    super.configureSourceViewerDecorationSupport( support );
    support.setCharacterPairMatcher( new HaskellCharacterPairMatcher() );
    String bracketsKey = EDITOR_MATCHING_BRACKETS;
    String colorKey = EDITOR_MATCHING_BRACKETS_COLOR;
    support.setMatchingCharacterPainterPreferenceKeys( bracketsKey, colorKey );
    support.setSymbolicFontName( getFontPropertyPreferenceKey() );
  }

  @Override
  protected String[] collectContextMenuPreferencePages() {
    List<String> ls=new ArrayList<String>(Arrays.asList( super.collectContextMenuPreferencePages()));
    ls.add( "net.sf.eclipsefp.haskell.ui.internal.preferences.editor.AppearancePP" );
    //ls.add("net.sf.eclipsefp.haskell.ui.internal.preferences.editor.AnnotationsPP");
    ls.add( "net.sf.eclipsefp.haskell.ui.internal.preferences.editor.SyntaxPP");
    ls.add("net.sf.eclipsefp.haskell.ui.internal.preferences.templates.HSCodeTemplatePreferences");
    return ls.toArray( new String[ls.size()] );
  }


  @Override
  protected String[] collectOverviewRulerMenuPreferencePages() {
    List<String> ls=new ArrayList<String>(Arrays.asList( super.collectOverviewRulerMenuPreferencePages()));
    ls.add( "net.sf.eclipsefp.haskell.ui.internal.preferences.editor.AppearancePP" );
    //ls.add("net.sf.eclipsefp.haskell.ui.internal.preferences.editor.AnnotationsPP");
    return ls.toArray( new String[ls.size()] );
  }

  @Override
  public void editorContextMenuAboutToShow( final IMenuManager menu ) {
    super.editorContextMenuAboutToShow( menu );
    if( isEditable() ) {
      IMenuManager mmSource = new MenuManager( UITexts.editor_actions_source, "source" ); //$NON-NLS-1$
      menu.prependToGroup( ITextEditorActionConstants.GROUP_EDIT, mmSource );

      mmSource.add( new Separator( "comments" ) ); //$NON-NLS-1$
      mmSource.add( new Separator( "formatting" ) ); //$NON-NLS-1$
      mmSource.add( new Separator( "organize" ) ); //$NON-NLS-1$

      addAction( mmSource, "comments", LINE_COMMENT_ACTION ); //$NON-NLS-1$
      addAction( mmSource, "comments", LINE_UNCOMMENT_ACTION ); //$NON-NLS-1$
      addAction( mmSource, "comments", COMMENT_PRAGMA_ACTION); //$NON-NLS-1$
      addAction( mmSource, "comments", HADDOCK_DOCUMENT_FOLLOWING_ACTION); //$NON-NLS-1$
      addAction( mmSource, "comments", HADDOCK_DOCUMENT_PREVIOUS_ACTION); //$NON-NLS-1$
      addAction( mmSource, "comments", HADDOCK_BLOCK_DOCUMENT_FOLLOWING_ACTION); //$NON-NLS-1$
      addAction( mmSource, "formatting", FORMAT_ACTION); //$NON-NLS-1$
    }
  }

  /**
   * get the location in display coordinates of the current selection
   * @return
   */
  public Point getSelectedPoint(){
    Point p2=getSourceViewer().getTextWidget().getLocationAtOffset( getSourceViewer().getTextWidget().getSelectionRange().x );
    return getSourceViewer().getTextWidget().toDisplay(p2 );
  }

  @Override
  protected void createActions() {
    super.createActions();

    // content assist
    String defId = ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS;
    setAction("ContentAssistProposal",
              createTextOpAction(this, "ContentAssistProposal", "ContentAssistProposal", ISourceViewer.CONTENTASSIST_PROPOSALS, defId )); //$NON-NLS-1$

    // comment/uncomment
    createTextOpAction(this, LINE_COMMENT_ACTION, commentResourcePrefix, ITextOperationTarget.PREFIX,
                        IEditorActionDefinitionIds.COMMENT );
    createTextOpAction(this, LINE_UNCOMMENT_ACTION, uncommentResourcePrefix, ITextOperationTarget.STRIP_PREFIX,
                        IEditorActionDefinitionIds.UNCOMMENT );

    // New actions that we contribute:
    ResourceBundle bundle = HaskellUIPlugin.getDefault().getResourceBundle();

    setAction(FORMAT_ACTION,
        new FormatAction( bundle, formatResourcePrefix + ".", this ));
    setAction(COMMENT_PRAGMA_ACTION,
              new PragmaCommentAction( bundle, commentPragmaResourcePrefix + ".", this ));
    setAction( HADDOCK_DOCUMENT_FOLLOWING_ACTION,
               new HaddockDocumentFollowingAction( bundle, "HaddockDocumentFollowing.", this ));
    setAction( HADDOCK_DOCUMENT_PREVIOUS_ACTION,
               new HaddockDocumentPreviousAction( bundle, "HaddockDocumentPrevious.", this ));
    setAction( HADDOCK_BLOCK_DOCUMENT_FOLLOWING_ACTION,
               new HaddockBlockDocumentFollowingAction( bundle, "HaddockDocumentBlockFollow.", this ));

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
                 IAction action=new SelectAnnotationForQuickFix(  HaskellEditor.this , (ProjectionViewer)getSourceViewer(), (MarkerAnnotation)ann );
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

    /**navigation actions
     * http://sourceforge.net/projects/eclipsefp/forums/forum/371922/topic/6192123
     * **/
    GotoAnnotationAction gf=new GotoAnnotationAction( this, true );
    gf.setActionDefinitionId( ITextEditorActionConstants.NEXT );
    setAction( ITextEditorActionConstants.NEXT, gf);
    markAsStateDependentAction( ITextEditorActionConstants.NEXT, true );
    getEditorSite().getActionBars().setGlobalActionHandler( ITextEditorActionConstants.NEXT, gf );

    GotoAnnotationAction gb=new GotoAnnotationAction( this, false );
    gb.setActionDefinitionId( ITextEditorActionConstants.PREVIOUS );
    setAction( ITextEditorActionConstants.PREVIOUS, gb);
    markAsStateDependentAction( ITextEditorActionConstants.PREVIOUS, true );
    getEditorSite().getActionBars().setGlobalActionHandler( ITextEditorActionConstants.PREVIOUS, gb );


  }


  @Override
  protected ISourceViewer createSourceViewer( final Composite parent, final IVerticalRuler ruler, final int styles ) {
    // copied this from the super class, replaced source viewer with
    // projection viewer
    fAnnotationAccess = createAnnotationAccess();
    fOverviewRuler = createOverviewRuler( getSharedColors() );
    ProjectionViewer viewer = new ProjectionViewer( parent, ruler, getOverviewRuler(), isOverviewRulerVisible(), styles );

    // ensure decoration support has been created and configured.
    getSourceViewerDecorationSupport( viewer );
    return viewer;
  }


  @Override
  public void createPartControl( final Composite parent ) {
    super.createPartControl( parent );

    ProjectionViewer projectionViewer = ( ProjectionViewer )getSourceViewer();
    projectionSupport = new ProjectionSupport( projectionViewer, getAnnotationAccess(), getSharedColors() );
    projectionSupport.install();
    projectionSupport.addSummarizableAnnotationType("org.eclipse.ui.workbench.texteditor.error"); //$NON-NLS-1$
    projectionSupport.addSummarizableAnnotationType("org.eclipse.ui.workbench.texteditor.warning"); //$NON-NLS-1$
    projectionViewer.doOperation( ProjectionViewer.TOGGLE );

    if( markOccurrencesComputer != null ) {
      ISelectionChangedListener listener = new ISelectionChangedListener() {

        @Override
        public void selectionChanged( final SelectionChangedEvent event ) {
          IDocument doc = getSourceViewer().getDocument();
          markOccurrencesComputer.setDocument( doc );
          new UIJob(UITexts.editor_occurrences_job) {
            @Override
            public IStatus runInUIThread(final IProgressMonitor monitor) {
              markOccurrencesComputer.compute();
              return Status.OK_STATUS;
           }
          }.schedule();
        }
      };
      projectionViewer.addPostSelectionChangedListener( listener );
    }

    activateContext();
  }


  private void activateContext() {
    IContextService contextService = ( IContextService )getSite().getService( IContextService.class );
    contextService.activateContext( CONTEXT_ID );
  }

  @Override
  public Object getAdapter( final Class required ) {
    Object result = null;
    // adapt the displayed source file to the outline viewer
    if( IContentOutlinePage.class.equals( required ) ) {
      if( outlinePage == null ) {
        //updateOutline();
          synchronize();
      }

      result = outlinePage;
    } else if( projectionSupport != null ) {
      result = projectionSupport.getAdapter( getSourceViewer(), required );
    }

    if( result == null ) {
      result = super.getAdapter( required );
    }
    return result;
  }

  // supplement some TextEditor functionality with specific handling
  // needed because we have an attached outline page
  // ///////////////////////////////////////////////////////////////

  @Override
  public void dispose() {
    if( outlinePage != null ) {
      outlinePage.setInput( null );
    }
    if (tokenScanner!=null){
      tokenScanner.dispose();
    }
//    if (instance != null) {
//      instance.removeListener( this );
//    }
    final IFile file=findFile();
    if (file!=null){
      HaskellCorePlugin.getModifiedByEditors().remove( file );
      final BWFacade f=BuildWrapperPlugin.getFacade( findFile().getProject() );
      if (f!=null){
        new Thread(new Runnable() {

          @Override
          public void run() {
            // synchronize and rebuild to be sure that we're in sync if we close a dirty editor
            f.synchronize1( file,true );
            f.build1( file );

          }
        }).start();

      }
    }

    super.dispose();
  }

  @Override
  public void doRevertToSaved() {
    super.doRevertToSaved();
    editorSaved();
  }


  @Override
  protected void editorSaved() {
    // Reload the file on the buildwrapper server side
    IFile file = findFile();
    if( file != null) {
      synchronize();
      BuildWrapperPlugin.getDefault().getUsageThread().addProject(file.getProject());
      if (tokenScanner!=null){
        tokenScanner.markTaskTags();
      }
    }
  }

  @Override
  public void init( final IEditorSite site, final IEditorInput input )
      throws PartInitException {
      super.init( site, input );
      outlinePage = new HaskellOutlinePage( this );
      if (lastOutlineResult!=null){
        outlinePage.setInput( lastOutlineResult.getOutlineDefs() );
      }
      IFile file = findFile();
      if (file!=null){
        BWFacade f=BuildWrapperPlugin.getFacade( findFile().getProject() );
        if (f!=null){
          f.synchronize1( file,true );
        }
      }
  }



  /**
   * Get the editor's current input file.
   *
   * @return An IFile object if the editor's input is a file, otherwise null.
   */
  public IFile findFile() {
    IEditorInput input = getEditorInput();
    if( input instanceof IFileEditorInput ) {
      return ( ( IFileEditorInput ) input ).getFile();
    }

    return null;
  }

  private String moduleName=null;
  public String getModuleName(){
    if (moduleName==null){
      moduleName=ResourceUtil.getModuleName( findFile() );
    }
    return moduleName;
  }

  public void setModuleName(final String module){
    moduleName=module;
  }

  @Override
  public void doSetInput( final IEditorInput input ) throws CoreException {

    super.doSetInput( input );
    IFile f=findFile();
    if (f!=null){
      HaskellCorePlugin.getModifiedByEditors().add( f );
    }
    // Ensure we synchronize to the correct file, which ought to have been set by the call to super.
    synchronize();
    // file may have been renamed
    moduleName=null;
  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.texteditor.StatusTextEditor#handleElementContentReplaced()
   */
  @Override
  protected void handleElementContentReplaced() {
    super.handleElementContentReplaced();
    synchronize();
  }

  // helping methods
  // ////////////////

  private boolean isAffectingProperty( final String property ) {
    return property.equals( EDITOR_COMMENT_COLOR )
        || property.equals( EDITOR_COMMENT_BOLD )
        || property.equals( EDITOR_DOC_COLOR )
        || property.equals( EDITOR_DOC_BOLD )
        || property.equals( EDITOR_PRAGMA_COLOR )
        || property.equals( EDITOR_PRAGMA_BOLD )
        || property.equals( EDITOR_LITERATE_COMMENT_COLOR )
        || property.equals( EDITOR_LITERATE_COMMENT_BOLD )
        || property.equals( EDITOR_DEFAULT_COLOR )
        || property.equals( EDITOR_DEFAULT_BOLD )
        || property.equals( EDITOR_FUNCTION_COLOR )
        || property.equals( EDITOR_FUNCTION_BOLD )
        || property.equals( EDITOR_KEYWORD_COLOR )
        || property.equals( EDITOR_KEYWORD_BOLD )
        || property.equals( EDITOR_STRING_COLOR )
        || property.equals( EDITOR_STRING_BOLD )
        || property.equals( EDITOR_CHAR_COLOR )
        || property.equals( EDITOR_CHAR_BOLD )
        || property.equals( EDITOR_NUMBER_COLOR )
        || property.equals( EDITOR_NUMBER_BOLD )
        || property.equals( EDITOR_VAR_COLOR )
        || property.equals( EDITOR_VAR_BOLD )
        || property.equals( EDITOR_CON_COLOR )
        || property.equals( EDITOR_CON_BOLD )
        || property.equals( EDITOR_SYMBOL_COLOR )
        || property.equals( EDITOR_SYMBOL_BOLD )
        || property.equals( EDITOR_CPP_COLOR )
        || property.equals( EDITOR_CPP_BOLD )
        || property.equals( EDITOR_TH_COLOR )
        || property.equals( EDITOR_TH_BOLD );
  }

  /**
   * Create a standard text operation action
   */
  public static TextOperationAction createTextOpAction( final TextEditor ed,final String actionIdName, final String resourcePrefix, final int targetId,
                                                  final String actionDefinitionId ) {
    ResourceBundle bundle = HaskellUIPlugin.getDefault().getResourceBundle();
    TextOperationAction action = new TextOperationAction( bundle, resourcePrefix + ".", ed, targetId );  //$NON-NLS-1$
    action.setActionDefinitionId( actionDefinitionId );
    ed.setAction( actionIdName, action );
    ed.markAsStateDependentAction( actionIdName, true );

    return action;
  }

  @Override
  public void setFocus() {
    super.setFocus();
    //IFile f=findFile();

    //HaskellUIPlugin.getDefault().getScionInstanceManager(f).backgroundTypecheckFile( f );
  }

  private void initMarkOccurrences() {
    // TODO TtC replace by something not Cohatoe-based
    /*
     * CohatoeServer server = CohatoeServer.getInstance(); IMarkOccurrences mo =
     * server.createFunction( IMarkOccurrences.class ); if( mo != null ) {
     * markOccurrencesComputer = new MarkOccurrenceComputer( this, mo ); }
     */
    markOccurrencesComputer = new MarkOccurrenceComputer( this);
  }


//  public void synchronize() {
//    IDocument document = getDocument();
//
//    if( markOccurrencesComputer != null ) {
//      WorkspaceJob occurances = new WorkspaceJob("Haskell occurance marker") {
//        @Override
//        public IStatus runInWorkspace( final IProgressMonitor monitor ) {
//          markOccurrencesComputer.compute();
//          return Status.OK_STATUS;
//        }
//      };
//
//      markOccurrencesComputer.setDocument( document );
//      occurances.schedule();
//    }
//  }

  /**
   * Update the outline page using the current editor file.
   */
//  public void updateOutline() {
//    updateOutline(findFile());
//  }

  private boolean needWrite=false;
  public void synchronize(){
    IFile file=findFile();
    if(importsManager!=null){
      importsManager.reset();
    }
    /*BWFacade f=BuildWrapperPlugin.getFacade( file.getProject() );
    if (f!=null){
      f.write( file,getDocument().get() );
    }*/
    if (file!=null && ResourceUtil.isInHaskellProject( file )){
      JobFacade jf=BuildWrapperPlugin.getJobFacade( file.getProject() );
      if (jf!=null){
        if (!needWrite && isDirty()){ // we need to write the docs if we're dirty
          needWrite=true;
        }
        jf.updateFromEditor( file, needWrite?getDocument():null, outlineHandler,this );
        if (!isDirty()){ // now we've written and not dirty
          needWrite=false;
        }
      } else {
        outlineHandler.handleOutline( new OutlineResult());
      }
    }
  }


  /**
   * @return the importsManager
   */
  public ImportsManager getImportsManager() {
    if(importsManager==null){
      importsManager=new ImportsManager(findFile(),getDocument(),names);
    }
    return importsManager;
  }

  /**
   * Update the outline page, using a specific file (notably when the editor's
   * input is changed.
   *
   * @param currentFile
   *          The current file from which the outline is generated.
   */
//  private void updateOutline(final IFile currentFile) {
//    if ( currentFile != null && outlinePage != null ) {
//      //ScionInstance instance = getInstance( currentFile );
//      JobFacade jf=BuildWrapperPlugin.getJobFacade( currentFile.getProject() );
//// getDocument(),
//      if (jf!=null){
//        jf.outline( currentFile, outlineHandler);
//      } else {
//        outlineHandler.handleOutline( Collections.<OutlineDef>emptyList() );
//      }
//    }
//  }

  public synchronized Location getOutlineLocation(final String name) {
    if ( defByName==null ){
      buildDefByName();
    }
    if (defByName!=null){
      List<OutlineDef> l = defByName.get( name );
      if (l!=null && l.size()>0){
        return l.iterator().next().getLocation();
      }
    }
    return null;
  }


  public boolean hasOutline() {
    return lastOutlineResult!=null;
  }


  public OutlineResult getLastOutlineResult() {
    return lastOutlineResult;
  }

//  public void showOutlineLocation(final String shortName){
//    Location location = getOutlineLocation( shortName );
//    if( location != null ) {
//      IDocument document = getDocument();
//      int startOffset = location.getStartOffset( document );
//      int length = location.getLength( document );
//      selectAndReveal( startOffset, length );
//    }
//  }

  public List<String> getLocalNames(){
    List<String> ls=new ArrayList<String>();
    if (outlinePage!=null && outlinePage.getInput()!=null){
      for (OutlineDef od:outlinePage.getInput()){
        ls.add(od.getName());
      }
    }
    return ls;
  }

  private void buildDefByName(){
    if (outlinePage!=null && outlinePage.getInput()!=null){

      defByName=new HashMap<String, List<OutlineDef>>();
      for (OutlineDef od:outlinePage.getInput()){
        buildDefByName(od);
      }
    }
  }

  private void buildDefByName(final OutlineDef od){
    List<OutlineDef> l=defByName.get( od.getName());
    if(l==null){
      l=new ArrayList<OutlineDef>();
      defByName.put( od.getName(), l );
    }
    l.add( od );
    for (OutlineDef od2:od.getChildren()){
      buildDefByName(od2);
    }
  }

  /**
   * is the start of the selection contained in an outline element
   * useful to find if a selection for a breakpoint is in real code
   */
  public boolean isInOutline(final ISelection sel){
    if (outlinePage!=null && outlinePage.getInput()!=null && sel instanceof ITextSelection){
      ITextSelection tsel=(ITextSelection)sel;
      int line=tsel.getStartLine()+1;
      for (OutlineDef od:outlinePage.getInput()){
        // here we could filter out if inside some type of outlinedef we do not want breakpoints

        if (od.getLocation().getStartLine()<=line && od.getLocation().getEndLine()>=line){
          return true;
        }
      }
    }
    return false;
  }


  public ScionTokenScanner getTokenScanner() {
    return tokenScanner;
  }


  public void setTokenScanner( final ScionTokenScanner tokenScanner ) {
    this.tokenScanner = tokenScanner;
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.buildwrapper.types.NameDefHandler#handleNameDefs(java.util.Collection)
   */
  @Override
  public void handleNameDefs( final Collection<NameDef> names ) {
    // we keep the old names if the build failed
    if (names!=null){
      this.names.clear();
      this.names.addAll( names );
    }

  }

  /**
   * @return the names
   */
  public Collection<NameDef> getNames() {
    return names;
  }

  // Interface methods for IScionEventListener

//  /**
//   * Process a scion-server status change event
//   *
//   * @param ev
//   *          The {@link ScionEvent} indicating the type of server event that
//   *          happened.
//   */
//  public void processScionServerEvent( final ScionEvent ev ) {
//    switch (ev.getEventType()) {
//      case EXECUTABLE_CHANGED: {
//        //final IFile file = findFile();
//        //if (file != null && ResourceUtil.isInHaskellProject( file ) ) {
//            //updateOutline( file );
//          synchronize();
//        //}
//        break;
//      }
//
//      default:
//        break;
//    }
//  }


}
