// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;
import net.sf.eclipsefp.haskell.core.halamo.IHaskellLanguageElement;
import net.sf.eclipsefp.haskell.core.halamo.ISourceLocation;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.scion.client.OutlineHandler;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.types.Location;
import net.sf.eclipsefp.haskell.scion.types.OutlineDef;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.HaskellCharacterPairMatcher;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.HaskellFoldingStructureProvider;
import net.sf.eclipsefp.haskell.ui.internal.editors.text.MarkOccurrenceComputer;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import net.sf.eclipsefp.haskell.ui.internal.resolve.SelectAnnotationForQuickFix;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.internal.views.outline.HaskellOutlinePage;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.jface.text.source.IVerticalRulerInfo;
import org.eclipse.jface.text.source.projection.ProjectionSupport;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.eclipse.ui.texteditor.MarkerAnnotation;
import org.eclipse.ui.texteditor.SourceViewerDecorationSupport;
import org.eclipse.ui.texteditor.TextOperationAction;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

/**
 * <p>
 * The main editor class for the Haskell editor.
 * </p>
 *
 * @author Leif Frenzel
 */
public class HaskellEditor extends TextEditor implements IEditorPreferenceNames {


  /**
   * <p>
   * the id under which the Haskell editor is declared.
   * </p>
   */
  public static final String ID = HaskellEditor.class.getName();

  /** The key binding context active while the Haskell editor is active */
  private static final String CONTEXT_ID = "net.sf.eclipsefp.haskell.ui.internal.editor.haskell.HaskellEditor.context";  //$NON-NLS-1$

  private HaskellOutlinePage outlinePage;
  private ProjectionSupport projectionSupport;
  private MarkOccurrenceComputer markOccurrencesComputer;
  private ScionInstance instance=null;
  private HaskellFoldingStructureProvider foldingStructureProvider;

  private List<OutlineDef> outline;
  private Map<String,List<OutlineDef>> defByName;

  public void reveal( final IHaskellLanguageElement element ) {
    Assert.isNotNull( element );
    IDocument doc = getSourceViewer().getDocument();
    ISourceLocation srcLoc = element.getSourceLocation();
    int offset = -1;
    try {
      offset = doc.getLineOffset( srcLoc.getLine() ) + srcLoc.getColumn();
    } catch( final BadLocationException badlox ) {
      // ignore
    }
    int length = element.getName().length();
    getSourceViewer().revealRange( offset, length );
  }

  public IDocument getDocument() {
    return getSourceViewer() == null ? null : getSourceViewer().getDocument();
  }


  // interface methods of TextEditor
  // ////////////////////////////////

  @Override
  protected void initializeEditor() {
    super.initializeEditor();
    setSourceViewerConfiguration( new HaskellConfiguration( this ) );
    setEditorContextMenuId( "#HaskellEditorContext" );  //$NON-NLS-1$
    // we configure the preferences ourselves
    setPreferenceStore( HaskellUIPlugin.getDefault().getPreferenceStore() );
    initMarkOccurrences();
    foldingStructureProvider=new HaskellFoldingStructureProvider( this );


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
  protected void configureSourceViewerDecorationSupport(
      final SourceViewerDecorationSupport support ) {
    super.configureSourceViewerDecorationSupport( support );
    support.setCharacterPairMatcher( new HaskellCharacterPairMatcher() );
    String bracketsKey = EDITOR_MATCHING_BRACKETS;
    String colorKey = EDITOR_MATCHING_BRACKETS_COLOR;
    support.setMatchingCharacterPainterPreferenceKeys( bracketsKey, colorKey );
    support.setSymbolicFontName( getFontPropertyPreferenceKey() );
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
      addAction( mmSource, "comments", "Comment" ); //$NON-NLS-1$ //$NON-NLS-2$
      addAction( mmSource, "comments", "Uncomment" ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  @Override
  protected void createActions() {
    super.createActions();

    // content assist
    String defId = ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS;
    createTextOpAction( "ContentAssistProposal", //$NON-NLS-1$
        ISourceViewer.CONTENTASSIST_PROPOSALS, defId );

    // comment/uncomment
    createTextOpAction( "Comment", ITextOperationTarget.PREFIX, //$NON-NLS-1$
        IActionDefinitionIds.COMMENT );
    createTextOpAction( "Uncomment", ITextOperationTarget.STRIP_PREFIX, //$NON-NLS-1$
        IActionDefinitionIds.UNCOMMENT );


    addRulerContextMenuListener( new IMenuListener() {

      public void menuAboutToShow( final IMenuManager manager ) {
       IVerticalRulerInfo service= (IVerticalRulerInfo)getAdapter(IVerticalRulerInfo.class);
       if (service!=null){
         int line=service.getLineOfLastMouseButtonActivity();

         for (Iterator<?> it=getSourceViewer().getAnnotationModel().getAnnotationIterator();it.hasNext();){
           Annotation ann=(Annotation)it.next();
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
  }


  @Override
  protected ISourceViewer createSourceViewer( final Composite parent,
      final IVerticalRuler ruler, final int styles ) {
    // copied this from the super class, replaced source viewer with
    // projection viewer
    fAnnotationAccess = createAnnotationAccess();
    fOverviewRuler = createOverviewRuler( getSharedColors() );
    ProjectionViewer viewer = new ProjectionViewer( parent, ruler,
        getOverviewRuler(), isOverviewRulerVisible(), styles );

    // ensure decoration support has been created and configured.
    getSourceViewerDecorationSupport( viewer );
    return viewer;
  }

  @Override
  public void createPartControl( final Composite parent ) {
    super.createPartControl( parent );
    ProjectionViewer projectionViewer = ( ProjectionViewer )getSourceViewer();
    projectionSupport = new ProjectionSupport( projectionViewer,
        getAnnotationAccess(), getSharedColors() );
    projectionSupport.install();
    projectionSupport.addSummarizableAnnotationType("org.eclipse.ui.workbench.texteditor.error"); //$NON-NLS-1$
    projectionSupport.addSummarizableAnnotationType("org.eclipse.ui.workbench.texteditor.warning"); //$NON-NLS-1$

    projectionViewer.doOperation( ProjectionViewer.TOGGLE );

    if( markOccurrencesComputer != null ) {
      ISelectionChangedListener listener = new ISelectionChangedListener() {

        public void selectionChanged( final SelectionChangedEvent event ) {
          IDocument doc = getSourceViewer().getDocument();
          markOccurrencesComputer.setDocument( doc );
          markOccurrencesComputer.compute();
        }
      };
      projectionViewer.addPostSelectionChangedListener( listener );
    }
    activateContext();


  }



  private void activateContext() {
    IContextService contextService = ( IContextService )getSite().getService(
        IContextService.class );
    contextService.activateContext( CONTEXT_ID );
  }

  @Override
  public Object getAdapter( final Class required ) {
    Object result = null;
    // adapt the displayed source file to the outline viewer
    if( IContentOutlinePage.class.equals( required ) ) {
      if( outlinePage == null ) {
        outlinePage = new HaskellOutlinePage( this );
      }
      if( outline != null ) {
        outlinePage.setInput( outline );
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

  // supplement some TextEditor funtionality with specific handling
  // needed because we have an attached outline page
  // ///////////////////////////////////////////////////////////////

  @Override
  public void dispose() {
    if( outlinePage != null ) {
      outlinePage.setInput( null );
    }
    super.dispose();
  }

  @Override
  public void doRevertToSaved() {
    super.doRevertToSaved();
    /*if( outlinePage != null ) {
      outlinePage.update();
    }*/
  }

  @Override
  public void doSave( final IProgressMonitor monitor ) {
    super.doSave( monitor );
    /*if( outlinePage != null ) {
      outlinePage.update();
    }*/
  }

  @Override
  public void doSaveAs() {
    super.doSaveAs();
    /*if( outlinePage != null ) {
      outlinePage.update();
    }*/
  }

  @Override
  protected void editorSaved() {
    // Reload the file on the Scion server side
    IFile file = findFile();
    if( file != null ) {
      HaskellUIPlugin.getDefault().getScionInstanceManager(file).reloadFile(file,new Runnable() {

        public void run() {
          synchronize();
        }
      });

    }
  }

  public IFile findFile() {
    IFile result = null;
    IEditorInput input = getEditorInput();
    if( input instanceof IFileEditorInput ) {
      result = ( ( IFileEditorInput )input ).getFile();
    }
    return result;
  }

  @Override
  public void doSetInput( final IEditorInput input ) throws CoreException {
    // unload the previous file from Scion
    IFile file = findFile();
    if (file != null && ResourceUtil.isInHaskellProject( file )){
      HaskellUIPlugin.getDefault().getScionInstanceManager(file).unloadFile(file);
    }
    instance=null;
    super.doSetInput( input );

    ScionInstance instance=getInstance();

    if (instance!=null) {
        file = findFile();
        instance.reloadFile( file,new Runnable() {

          public void run() {
           synchronize();

          }
        });
    }
  }

  /**
   * get the scion instance, creating it if needed
   */
  private ScionInstance getInstance(){
    if (instance==null){
      IFile file = findFile();
      // load the new file into Scion
      if (file != null && ResourceUtil.isInHaskellProject( file )) {
        //HaskellUIPlugin.getDefault().getScionInstanceManager(file).loadFile(file);
        instance=HaskellUIPlugin.getDefault().getScionInstanceManager(file);
      }
    }
    return instance;
  }

  // helping methods
  // ////////////////

  private boolean isAffectingProperty( final String property ) {
    return property.equals( EDITOR_COMMENT_COLOR )
        || property.equals( EDITOR_COMMENT_BOLD )
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
        || property.equals( EDITOR_CHAR_BOLD );
  }

  private void createTextOpAction( final String name, final int targetId,
      final String actionDefinitionId ) {
    ResourceBundle bundle = HaskellUIPlugin.getDefault().getResourceBundle();
    Action action = new TextOperationAction( bundle, name + ".", this, targetId );  //$NON-NLS-1$
    action.setActionDefinitionId( actionDefinitionId );
    setAction( name, action );
    markAsStateDependentAction( name, true );
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
  }

  public void synchronize(){
    IDocument document=getDocument();

    foldingStructureProvider.setDocument( document );
    if( markOccurrencesComputer != null ) {
      markOccurrencesComputer.setDocument( document );
    }

    Shell shell = getSite().getShell();
    if( shell != null && !shell.isDisposed() ) {
      shell.getDisplay().asyncExec( new Runnable() {
        public void run() {
          if( markOccurrencesComputer != null ) {
            markOccurrencesComputer.compute();
          }
        }
      } );

      ScionInstance instance=getInstance();
      if( instance!=null && instance.isLoaded( findFile() )) {
        instance.outline(findFile(),new OutlineHandler() {

          public void outlineResult( final List<OutlineDef> outlineDefs ) {
            outline=outlineDefs;
            if(getOutlinePage()!=null){
              getOutlinePage().setInput( outlineDefs );
            }
           foldingStructureProvider.updateFoldingRegions(outlineDefs);
          }
        });
      } else {
        List<OutlineDef> outlineDefs=Collections.emptyList();
        if(getOutlinePage()!=null){
          getOutlinePage().setInput( outlineDefs );
        }
        foldingStructureProvider.updateFoldingRegions(outlineDefs);
      }
    }
  }

  public HaskellOutlinePage getOutlinePage() {
    return outlinePage;
  }

  public synchronized Location getOutlineLocation(final String name){
    if (defByName==null){
      buildDefByName();
    }
    List<OutlineDef> l=defByName.get( name);
    if (l!=null && l.size()>0){
      return l.iterator().next().getLocation();
    }
    return null;
  }

  private void buildDefByName(){
    if (outline!=null){
      defByName=new HashMap<String, List<OutlineDef>>();
      for (OutlineDef od:outline){
        List<OutlineDef> l=defByName.get( od.getName());
        if(l==null){
          l=new ArrayList<OutlineDef>();
          defByName.put( od.getName(), l );
        }
        l.add( od );
      }
    }
  }
}