// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.views.coview;

import org.eclipse.jface.action.*;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.text.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.*;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.part.ViewPart;

import de.leiffrenzel.fp.haskell.ui.util.HaskellUIImages;
import de.leiffrenzel.fp.haskell.ui.util.IImageNames;

/** <p>The workbench view for compiler output. A simple text-displaying 
  * viewer.</p> 
  * 
  * @author Leif Frenzel
  */
public class CompilerOutputView extends ViewPart {
  
  public static final String ID = CompilerOutputView.class.getName(); 

  IWorkbenchPart part = null;
  
  private ITextViewer textViewer;
  private Action copyAction;
  private Action selectAllAction;
  private Action clearAction;
  
  
  public void createPartControl( final Composite parent ) {
    textViewer = new TextViewer( parent, SWT.NONE );
    textViewer.setDocument( new CompilerOutputDocument() );
    textViewer.setEditable( false );
    createActions();
    createContextMenu();
    hookGlobalActions();
  }

  public void setFocus() {
    textViewer.getTextWidget().setFocus();
  }

  
  // helping methods
  //////////////////
  
  private void createActions() {
    copyAction = new Action( "Copy" ) {
      public void run() {
        copy();
      }
    };
    
    selectAllAction = new Action( "Select All" ) {
      public void run() {
        selectAll();
      }
    };

    clearAction = new Action( "Clear View" ) {
      public void run() {
        clearDocument();
      }
    };
    String key = IImageNames.CO_VIEW_CLEAR;
    ImageDescriptor clearImageDesc = HaskellUIImages.getImageDescriptor( key );
    clearAction.setImageDescriptor( clearImageDesc );
    
    IToolBarManager mgr = getViewSite().getActionBars().getToolBarManager();
    mgr.add( clearAction );
  }

  private void createContextMenu() {
    final MenuManager menuManager = new MenuManager();
    menuManager.setRemoveAllWhenShown( true );
    menuManager.addMenuListener( new IMenuListener() {
      public void menuAboutToShow( final IMenuManager manager ) {
        fillContextMenu( menuManager );
      }
    } );
    Menu menu = menuManager.createContextMenu( textViewer.getTextWidget() );
    textViewer.getTextWidget().setMenu( menu );
    getSite().registerContextMenu( menuManager, new DummySelectionProvider() );
  }

  private void fillContextMenu( final IMenuManager menuManager ) {
    menuManager.add( copyAction );
    String additionsId = IWorkbenchActionConstants.MB_ADDITIONS;
    menuManager.add( new GroupMarker( additionsId ) );
    menuManager.add( selectAllAction );
    menuManager.add( new Separator() );
    menuManager.add( clearAction );
  }
  
  private void clearDocument() {
    IDocument document = textViewer.getDocument();
    if( document != null ) {
      document.set( "" );
    }
  }
  
  private void selectAll() {
    textViewer.setSelectedRange( 0, textViewer.getBottomIndexEndOffset() + 1 );
  }
  
  private void copy() {
    ITextOperationTarget target = textViewer.getTextOperationTarget();
    if( target != null ) {
      target.doOperation( ITextOperationTarget.COPY );
    }
  }
  
  private void hookGlobalActions() {
    IActionBars bars = getViewSite().getActionBars();
    bars.setGlobalActionHandler( ActionFactory.SELECT_ALL.getId(), 
                                 selectAllAction );
    bars.setGlobalActionHandler( ActionFactory.COPY.getId(), 
                                 copyAction );
  }

  
  // inner classes
  ////////////////
  
  /** a dummy selection provider */
  private class DummySelectionProvider implements ISelectionProvider {
    public void addSelectionChangedListener( 
                                    final ISelectionChangedListener listener ) {
      // implemented empty
    }

    public void removeSelectionChangedListener( 
                                    final ISelectionChangedListener listener ) {
      // implemented empty
    }
    
    public ISelection getSelection() {
      return new ISelection() {
        public boolean isEmpty() {
          return true;
        }
      };
    }

    public void setSelection( final ISelection selection ) {
      // implemented empty
    }
  }
}