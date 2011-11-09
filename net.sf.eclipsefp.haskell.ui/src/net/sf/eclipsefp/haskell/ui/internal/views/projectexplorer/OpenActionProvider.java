package net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.internal.views.outline.HaskellOutlinePage;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.navigator.CommonActionProvider;
import org.eclipse.ui.navigator.ICommonActionConstants;
import org.eclipse.ui.navigator.ICommonActionExtensionSite;
import org.eclipse.ui.navigator.ICommonMenuConstants;
import org.eclipse.ui.navigator.ICommonViewerSite;
import org.eclipse.ui.navigator.ICommonViewerWorkbenchSite;
import org.eclipse.ui.part.FileEditorInput;

/**
 * <p>Action Provider for opening outline components from project explorer</p>
  *
  * @author JP Moresmau
 */
public class OpenActionProvider extends CommonActionProvider {

  private OpenComponentAction openAction;

  @Override
  public void init( final ICommonActionExtensionSite aSite ) {
    super.init( aSite );
    ICommonViewerSite viewSite = aSite.getViewSite();
    if( viewSite instanceof ICommonViewerWorkbenchSite ) {
      ICommonViewerWorkbenchSite wSite = ( ICommonViewerWorkbenchSite )viewSite;
      openAction = new OpenComponentAction( wSite.getPage(), wSite
          .getSelectionProvider() );
    }
  }

  @Override
  public void fillContextMenu( final IMenuManager menu ) {
    if( openAction != null && openAction.isEnabled() ) {
      menu.appendToGroup( ICommonMenuConstants.GROUP_OPEN, openAction );
    }
  }


  @Override
  public void fillActionBars( final IActionBars actionBars ) {
    if( openAction != null && openAction.isEnabled() ) {
      actionBars.setGlobalActionHandler( ICommonActionConstants.OPEN,
          openAction );
    }
  }

  private static class OpenComponentAction extends Action {

    private final IWorkbenchPage page;
    private final ISelectionProvider selectionProvider;
    private ProjectExplorerOutlineDef def;
    private ProjectExplorerStanza stanza;

    private OpenComponentAction( final IWorkbenchPage p,
        final ISelectionProvider selProvider ) {
      super( UITexts.explorer_outline_open );
      this.page = p;
      this.selectionProvider = selProvider;
    }

    @Override
    public boolean isEnabled() {
      ISelection selection = selectionProvider.getSelection();
      def = null;
      stanza=null;
      if( selection != null && !selection.isEmpty() ) {
        IStructuredSelection ss = ( ( IStructuredSelection )selection );
        if( ss.size() == 1 ) {
          Object o = ss.getFirstElement();
          if( o instanceof ProjectExplorerOutlineDef ) {
            def = ( ProjectExplorerOutlineDef )o;
          } else if (o instanceof ProjectExplorerStanza){
            stanza=(ProjectExplorerStanza)o;
          }
        }
      }
      return def != null || stanza!=null;
    }

   /* @Override
    public String getActionDefinitionId() {
      return "net.sf.eclipsefp.haskell.ui.openCommand"; //$NON-NLS-1$
    }
*/


    @Override
    public void run() {
      try {
        if (def!=null){

            IEditorPart p = page.openEditor( new FileEditorInput( def.getOwner() ),
                HaskellEditor.ID );
            HaskellOutlinePage.reveal( ( HaskellEditor )p, def.getOutlineDef() );

        } else if (stanza!=null){
          CabalFormEditor e=(CabalFormEditor)page.openEditor( new FileEditorInput( stanza.getOwner() ), CabalFormEditor.ID );
          e.getCabalSourceEditor().selectAndReveal( stanza );
        }
      } catch( Exception e ) {
        HaskellUIPlugin.log( e );
      }
    }
  }

}
