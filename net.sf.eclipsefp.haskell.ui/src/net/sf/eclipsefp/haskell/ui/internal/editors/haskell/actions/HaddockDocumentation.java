package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.actions;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.actions.HaskellActionConstants;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.commands.ActionHandler;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.events.MenuAdapter;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IPartService;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowPulldownDelegate2;
import org.eclipse.ui.handlers.IHandlerService;

/**
 * Dynamic Source/commentGroup menu for managing Haddock-style code documentation. Much of this code was gratuitously kiped
 * out of the Java editor's source.
  *
  * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public class HaddockDocumentation implements IWorkbenchWindowPulldownDelegate2 {
  /** The submenu attached to the Haddock documentation menu */
  private Menu fMenu;
  /** Workbench handler service */
  private IHandlerService fHandlerService;
  /** Workbench part/component activation service */
  private IPartService fPartService;
  /** Lifecycle event listener */
  private final IPartListener fPartListener;

  /** Default constructor. */
  public HaddockDocumentation() {
    super();
    fHandlerService = null;
    fPartService = null;
    fPartListener = new IPartListener() {
      /** {@inheritDoc} */
      public void partActivated(final IWorkbenchPart part) {
        // NOP
      }
      /** {@inheritDoc} */
      public void partBroughtToTop(final IWorkbenchPart part) {
        // NOP
      }
      /** {@inheritDoc} */
      public void partClosed(final IWorkbenchPart part) {
        // NOP
      }
      /** {@inheritDoc} */
      public void partDeactivated(final IWorkbenchPart part) {
        disposeMenuItems();
      }
      /** {@inheritDoc} */
      public void partOpened(final IWorkbenchPart part) {
        // NOP
      }
    };
  }

  /** {@inheritDoc} */
  public void run( final IAction action ) {
    // NOP, for now
  }

  /** {@inheritDoc} */
  public void selectionChanged( final IAction action, final ISelection selection ) {
    // NOP
  }

  /** {@inheritDoc} */
  public void dispose() {
    if (fPartService != null) {
      fPartService.removePartListener(fPartListener);
      fPartService = null;
    }
    setMenu(null);
  }

  public void init( final IWorkbenchWindow window ) {
    if (fPartService != null) {
      fPartService.removePartListener(fPartListener);
      fPartService = null;
    }

    if (window != null) {
      IPartService partService= window.getPartService();
      if (partService != null) {
        fPartService = partService;
        partService.addPartListener(fPartListener);
      }

      fHandlerService = (IHandlerService) window.getService( IHandlerService.class );
    }
  }

  /**
   * Cleanup SWT resources
   */
  protected void disposeMenuItems() {
    if (fMenu == null || fMenu.isDisposed()) {
      return;
    }
    MenuItem[] items = fMenu.getItems();
    for (int i=0; i < items.length; i++) {
      MenuItem menuItem= items[i];
      if (!menuItem.isDisposed()) {
        menuItem.dispose();
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  public Menu getMenu( final Control parent ) {
    setMenu(new Menu(parent));
    fillMenu(fMenu);
    initMenu();
    return fMenu;
  }

  /**
   * {@inheritDoc}
   */
  public Menu getMenu( final Menu parent ) {
    setMenu(new Menu(parent));
    fillMenu(fMenu);
    initMenu();
    return fMenu;
  }

  /**
   * Additional menu initialization. Installs a MenuAdapter listener to dispose of menu items and re-initializing the menu
   * before the menu is shown. This ensures that the menu, if it ever is context-sensitive, is properly initialized for
   * to the editor's contents.
   */
  protected void initMenu() {
    fMenu.addMenuListener(new MenuAdapter() {
      @Override
      public void menuShown(final MenuEvent e) {
        Menu m = (Menu) e.widget;
        MenuItem[] items = m.getItems();
        for (int i=0; i < items.length; i++) {
          items[i].dispose();
        }
        fillMenu(m);
      }
    });
  }

  /**
   * The menu to show in the workbench menu
   * @param menu the menu to fill entries into it
   */
  protected void fillMenu(final Menu menu) {

    IWorkbenchPart activePart= HaskellUIPlugin.getActivePage().getActivePart();
    if (!(activePart instanceof HaskellEditor)) {
      ActionContributionItem item= new ActionContributionItem(NONE_APPLICABLE_ACTION);
      item.fill(menu, -1);
      return;
    }

    HaskellEditor editor= (HaskellEditor) activePart;

    ActionContributionItem docPreviousAction = new ActionContributionItem(new HaddockDocumentPreviousAction(editor));
    docPreviousAction.fill(menu, -1);
    ActionContributionItem docFollowingAction = new ActionContributionItem(new HaddockDocumentFollowingAction(editor));
    docFollowingAction.fill(menu, -1);

    docPreviousAction.update();
    docFollowingAction.update();

    IWorkbenchPartSite site = activePart.getSite();
    IHandlerService handlerSvc = (IHandlerService) site.getService( IHandlerService.class );

    if (handlerSvc != null) {
      handlerSvc.activateHandler( HaskellActionConstants.HADDOCK_FOLLOWING_DEF_ID, new ActionHandler(docFollowingAction.getAction()) );
    }
  }

  /**
   * Install a new menu.
   */
  private void setMenu(final Menu menu) {
    if (fMenu != null) {
      fMenu.dispose();
    }
    fMenu = menu;
  }

  /** A stock menu item when nothing is applicable */
  private static Action NONE_APPLICABLE_ACTION = new Action(UITexts.HaddockDocumentation_not_applicable) {
    @Override
    public void run() {
      //Do nothing
    }
    @Override
    public boolean isEnabled() {
      return false;
    }
  };
}
