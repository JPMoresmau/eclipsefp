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
import org.eclipse.ui.plugin.AbstractUIPlugin;

/**
 * <p>Action Provider for opening outline components from project explorer</p>
  *
  * @author JP Moresmau
 */
public class OpenActionProvider extends CommonActionProvider {

  private OpenHaskellComponentAction openHaskellAction;
  private OpenCabalComponentAction openCabalAction;

  @Override
  public void init( final ICommonActionExtensionSite aSite ) {
    super.init( aSite );
    ICommonViewerSite viewSite = aSite.getViewSite();
    if( viewSite instanceof ICommonViewerWorkbenchSite ) {
      ICommonViewerWorkbenchSite wSite = ( ICommonViewerWorkbenchSite )viewSite;
      openHaskellAction = new OpenHaskellComponentAction( wSite.getPage(), wSite
          .getSelectionProvider() );
      openCabalAction = new OpenCabalComponentAction( wSite.getPage(), wSite
          .getSelectionProvider() );
    }
  }

  @Override
  public void fillContextMenu( final IMenuManager menu ) {
    if( openHaskellAction != null && openHaskellAction.isEnabled() ) {
      menu.appendToGroup( ICommonMenuConstants.GROUP_OPEN, openHaskellAction );
    }
    if( openCabalAction != null && openCabalAction.isEnabled() ) {
      menu.appendToGroup( ICommonMenuConstants.GROUP_OPEN, openCabalAction );
    }
  }


  @Override
  public void fillActionBars( final IActionBars actionBars ) {
    if( openHaskellAction != null && openHaskellAction.isEnabled() ) {
      actionBars.setGlobalActionHandler( ICommonActionConstants.OPEN,
          openHaskellAction );
    } else if( openCabalAction != null && openCabalAction.isEnabled() ) {
      actionBars.setGlobalActionHandler( ICommonActionConstants.OPEN,
          openCabalAction );
    }
  }

  private static abstract class AbstractOpenComponentAction extends Action {
    protected final IWorkbenchPage page;
    private final ISelectionProvider selectionProvider;

    private AbstractOpenComponentAction(final String name,final String icon, final IWorkbenchPage p,
        final ISelectionProvider selProvider ) {
      super( name,AbstractUIPlugin.imageDescriptorFromPlugin( HaskellUIPlugin.getPluginId(), icon ));
      this.page = p;
      this.selectionProvider = selProvider;
    }

    @Override
    public boolean isEnabled() {
      ISelection selection = selectionProvider.getSelection();

      if( selection != null && !selection.isEmpty() ) {
        IStructuredSelection ss = ( ( IStructuredSelection )selection );
        if( ss.size() == 1 ) {
          Object o = ss.getFirstElement();
          return isEnabled( o );
        }
      }
      return false;
    }

    protected abstract boolean isEnabled(Object o);
  }

  private static class OpenHaskellComponentAction extends AbstractOpenComponentAction {

    private ProjectExplorerOutlineDef def;

    private OpenHaskellComponentAction( final IWorkbenchPage p,
        final ISelectionProvider selProvider ) {
      super( UITexts.explorer_outline_open, "icons/obj16/hsfile_obj.gif",p,selProvider );

    }

    @Override
    protected boolean isEnabled(final Object o) {
      def = null;

      if( o instanceof ProjectExplorerOutlineDef ) {
        def = ( ProjectExplorerOutlineDef )o;
      }
      return def != null;
    }

    @Override
    public void run() {
      try {
        if (def!=null){

            IEditorPart p = page.openEditor( new FileEditorInput( def.getOwner() ),
                HaskellEditor.ID );
            HaskellOutlinePage.reveal( ( HaskellEditor )p, def.getOutlineDef() );

        }
      } catch( Exception e ) {
        HaskellUIPlugin.log( e );
      }
    }
  }

  private static class OpenCabalComponentAction extends AbstractOpenComponentAction {

    private ProjectExplorerStanza stanza;

    private OpenCabalComponentAction( final IWorkbenchPage p,
        final ISelectionProvider selProvider ) {
      super( UITexts.explorer_outline_open_cabal, "icons/eview16/cabal.gif",p,selProvider );

    }

    @Override
    protected boolean isEnabled(final Object o) {
      stanza=null;
      if (o instanceof ProjectExplorerStanza){
            stanza=(ProjectExplorerStanza)o;
      }
      return stanza!=null;
    }


    @Override
    public void run() {
      try {
        if (stanza!=null){
          CabalFormEditor e=(CabalFormEditor)page.openEditor( new FileEditorInput( stanza.getOwner() ), CabalFormEditor.ID );
          e.selectAndReveal( stanza.getStanza() );
        }
      } catch( Exception e ) {
        HaskellUIPlugin.log( e );
      }
    }
  }
}
