// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.mbview;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.ui.IMemento;

import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;


/** <p>encapsulates all state handling of the user interrface of the 
  * Module browser, especially preserving that state between user 
  * sessions.</p>
  * 
  * @author Leif Frenzel
  */
class UIState implements IUIState {

  // memento tag names
  private static final String TAG_SELECTION       = "selection";
  private static final String TAG_EXPANDED        = "expanded";
  private static final String TAG_ELEMENT         = "element";
  private static final String TAG_PATH            = "path";
  private static final String TAG_LAYOUT          = "layout";
  private static final String TAG_FILTER_CRITERIA = "filters";
  private static final String TAG_CRITERION       = "criterion";
  private static final String TAG_ID              = "id";
  

  /** the value for 'Flat' of the memento tag for the layout */
  private static final int FLAT = 1;
  /** the value for 'Hierarchical' of the memento tag for the layout */
  private static final int HIERARCHICAL = 2;
  /** the currently set layout type (flat or hierarchical). */
  private int layout = FLAT;

  private final ModuleBrowser moduleBrowser;
  private TreeViewer viewer;

  /** the memento from which this UIState was initialized. */
  private final IMemento memento;
  
  UIState( final ModuleBrowser moduleBrowser, final IMemento memento ) {
    this.moduleBrowser = moduleBrowser;
    this.memento = memento;
  }
  
  void applyTo( final TreeViewer viewer ) {
    this.viewer = viewer;
    if( memento != null ) {
      restoreLayoutState();
      restoreExpansionState();
      restoreSelectionState();
      restoreFilterState();
      moduleBrowser.refreshViewer();
    }
  }

  void saveState( final IMemento memento ) {
    if( viewer == null ) {
      // part has not been created
      if( this.memento != null ) { 
        // Keep the old state
        memento.putMemento( this.memento );
      }
    } else {
      saveExpansionState( memento );
      saveSelectionState( memento );
      saveLayoutState( memento );
      saveFilterState( memento );
    }
  }
  
  
  // interface methods of IUIState
  ////////////////////////////////

  public boolean isFlatLayout() {
    return layout == FLAT;
  }
  
  public void toggleLayout() {
    layout = ( layout == FLAT ) ? HIERARCHICAL : FLAT;
    moduleBrowser.refreshViewer();
  }
  
  
  // helping methods
  //////////////////

  private void restoreFilterState() {
    IMemento childMem = memento.getChild( TAG_FILTER_CRITERIA );
    if( childMem != null ) {
      List list = new ArrayList();
      IMemento[] elementMem = childMem.getChildren( TAG_CRITERION );
      for( int i = 0; i < elementMem.length; i++ ) {
        String id = elementMem[ i ].getString( TAG_ID );
        FilterCriterion criterion = FilterCriterion.create( id );
        if( criterion != null ) {
          list.add( criterion );
        }
      }
      moduleBrowser.getFilter().setActiveCriteria( list.toArray() );
    }
  }
  
  private void saveFilterState( final IMemento memento ) {
    if( memento != null ) {
      Object[] criteria = moduleBrowser.getFilter().getActiveCriteria();
      if( criteria.length > 0 ) {
        IMemento elementMemento = memento.createChild( TAG_FILTER_CRITERIA );
        for( int i = 0; i < criteria.length; i++ ) {
          IMemento elementMem = elementMemento.createChild( TAG_CRITERION );
          FilterCriterion criterion = ( FilterCriterion )criteria[ i ];
          if( criterion != null ) {
            elementMem.putString( TAG_ID, criterion.getId() );
          }
        }
      }
    }
  }
  
  private void restoreLayoutState() {
    Integer intFromMemento = memento.getInteger( TAG_LAYOUT );
    if( intFromMemento != null ) {
      layout = intFromMemento.intValue();
    }
  }

  private void saveLayoutState( final IMemento memento ) {
    if( memento != null ) {
      memento.putInteger( TAG_LAYOUT, layout );
    }     
  }

  private void restoreExpansionState() {
    IMemento childMem = memento.getChild( TAG_EXPANDED );
    if( childMem != null ) {
      viewer.setExpandedElements( readElements( childMem ).toArray() );
    }
  }
  
  private void saveExpansionState( final IMemento memento ) {
    Object expandedElements[] = viewer.getVisibleExpandedElements();
    saveResources( expandedElements, memento, TAG_EXPANDED );
  }

  private void restoreSelectionState() {
    IMemento childMem = memento.getChild( TAG_SELECTION );
    if( childMem != null ) {
      List list = readElements( childMem );
      viewer.setSelection( new StructuredSelection( list ) );
    }
  }
  
  private List readElements( final IMemento childMem ) {
    List list = new ArrayList();
    IMemento[] elementMem = childMem.getChildren( TAG_ELEMENT );
    for( int i = 0; i < elementMem.length; i++ ) {
      String entry = elementMem[ i ].getString( TAG_PATH );
      Object element = getWsRoot().findMember( entry );
      if( element != null ) {
        // TODO this is only a temporary solution, there may be other element
        // types, which are not themselves resources, in the module browser,
        // so we need a generic mechanism here
        if(    element instanceof IProject
            && ( ( IProject )element ).isOpen() ) {
          list.add( HaskellProjectManager.get( ( IProject ) element ) );
        } else {
          list.add( element );
        }
      }
    }
    return list;
  }

  private IWorkspaceRoot getWsRoot() {
    return ResourcesPlugin.getWorkspace().getRoot();
  }

  private void saveSelectionState( final IMemento memento ) {
    IStructuredSelection sel = ( IStructuredSelection )viewer.getSelection();
    saveResources( sel.toArray(), memento, TAG_SELECTION );
  }
  
  private void saveResources( final Object[] elements, 
                              final IMemento memento,
                              final String tag ) {
    if( elements.length > 0 ) {
      IMemento elementMemento = memento.createChild( tag );
      for( int i = 0; i < elements.length; i++ ) {
        IMemento elementMem = elementMemento.createChild( TAG_ELEMENT );
        IResource resource = ResourceUtil.findResource( elements[ i ] );
        if( resource != null ) {
          elementMem.putString( TAG_PATH, resource.getFullPath().toString() );
        }
      }
    }
  }
}