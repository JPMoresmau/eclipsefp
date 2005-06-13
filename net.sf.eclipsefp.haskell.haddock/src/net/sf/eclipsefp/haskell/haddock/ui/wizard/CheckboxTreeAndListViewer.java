// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.ui.wizard;


import java.util.*;
import java.util.List;

import org.eclipse.jface.viewers.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;

/**
 *  Combines a CheckboxTreeViewer and CheckboxListViewer.
 *  All viewer selection-driven interactions are handled within this viewer
 *  
 *  taken and refactored from org.eclipse.jdt.internal.ui.jarpackager
 */
public class CheckboxTreeAndListViewer implements ICheckStateListener, 
                                                  ISelectionChangedListener {

  private Object fRoot;
  private Object fCurrentTreeSelection;
  private List fExpandedTreeNodes= new ArrayList();
  private Map fCheckedStateStore= new HashMap(9);
  private List fWhiteCheckedTreeItems= new ArrayList();
  private List fListeners= new ArrayList();

  private ITreeContentProvider fTreeContentProvider;
  private IStructuredContentProvider fListContentProvider;
  private ILabelProvider fTreeLabelProvider;
  private ILabelProvider fListLabelProvider;

  // UI elements
  private CheckboxTreeViewer treeViewer;
  private CheckboxTableViewer listViewer;

  /** Creates an instance of this class.  Use this constructor if you wish to 
    * specify the width and/or height of the combined widget (to only hardcode 
    * one of the sizing dimensions, specify the other dimension's value as -1)
    */
  public CheckboxTreeAndListViewer( final Composite parent,
                                    final Object rootObject,
                                    final ITreeContentProvider treeCP,
                                    final ILabelProvider treeLabelProvider,
                                    final IStructuredContentProvider listCP,
                                    final ILabelProvider listLabelProvider,
                                    final int width,
                                    final int height ) {
    fRoot = rootObject;
    fTreeContentProvider = treeCP;
    fListContentProvider = listCP;
    fTreeLabelProvider = treeLabelProvider;
    fListLabelProvider = listLabelProvider;
    createContents( parent, width, height );
    initialize();
  }
  
  /**
   * This method must be called just before this window becomes visible.
   */
  public void aboutToOpen() {
    determineWhiteCheckedDescendents(fRoot);
    checkNewTreeElements(getTreeChildren(fRoot));
    fCurrentTreeSelection= null;

    //select the first element in the list
    Object[] elements= getTreeChildren(fRoot);
    Object primary= elements.length > 0 ? elements[0] : null;
    if (primary != null) {
      treeViewer.setSelection(new StructuredSelection(primary));
    }
    treeViewer.getControl().setFocus();
  }

  
  /**
   * Adds the receiver and all of it's ancestors to the checkedStateStore 
   * if they are not already there.
   */
  private void addToHierarchyToCheckedStore( final Object treeElement ) {
    // if this tree element is already gray then its ancestors all are as well
    if( !fCheckedStateStore.containsKey( treeElement ) ) {
      fCheckedStateStore.put( treeElement, new ArrayList() );
    }
    Object parent = fTreeContentProvider.getParent( treeElement );
    if( parent != null ) {
      addToHierarchyToCheckedStore( parent );
    }
  }
  /**  Returns a boolean indicating whether all children of the passed tree 
   * element are currently white-checked */
  protected boolean areAllChildrenWhiteChecked( final Object treeElement ) {
    boolean result = true;
    Object[] children = getTreeChildren( treeElement );
    for( int i = 0; result && i < children.length; ++i ) {
      if( !fWhiteCheckedTreeItems.contains( children[ i ] ) ) {
        result = false;
      }
    }
    return result;
  }
  
  /** Returns whether all list elements associated with
   *  the passed tree element are currently checked */
  protected boolean areAllElementsChecked( final Object treeElement ) {
    List checkedElements= (List)fCheckedStateStore.get(treeElement);
    boolean result = false;
    if( checkedElements != null ) {
      result = getListItemsSize(treeElement) == checkedElements.size();
    }
    return result;
  }
  /**
   *  Iterates through the passed elements which are being realized for the 
   *  first time and check each one in the tree viewer as appropriate
   */
  protected void checkNewTreeElements( final Object[] elements ) {
    for (int i= 0; i < elements.length; ++i) {
      Object currentElement= elements[i];
      boolean checked= fCheckedStateStore.containsKey(currentElement);
      treeViewer.setChecked(currentElement, checked);
      treeViewer.setGrayed(
        currentElement,
        checked && !fWhiteCheckedTreeItems.contains(currentElement));
    }
  }
  /**
  * An item was checked in one of self's two views.  Determine which
  * view this occurred in and delegate appropriately
  *
  * @param event CheckStateChangedEvent
  */
  public void checkStateChanged(final CheckStateChangedEvent event) {

    //Potentially long operation - show a busy cursor
    BusyIndicator.showWhile( treeViewer.getControl().getDisplay(), 
                             new Runnable() {
      public void run() {
        if (event.getCheckable().equals(treeViewer)) {
          treeItemChecked(event.getElement(), event.getChecked());
        } else {
          listItemChecked(event.getElement(), event.getChecked(), true);
        }
        notifyCheckStateChangeListeners(event);
      }
    });
  }

  /**
   * Returns a boolean indicating whether the passed tree element should be
   * at LEAST gray-checked.  Note that this method does not consider whether
   * it should be white-checked, so a specified tree item which should be
   * white-checked will result in a <code>true</code> answer from this method.
   * To determine whether a tree item should be white-checked use method
   * #determineShouldBeWhiteChecked(Object).
   */
  protected boolean determineShouldBeAtLeastGrayChecked( final Object elem ) {
    boolean result = false;
    // if any list items associated with treeElement are checked then it
    // retains its gray-checked status regardless of its children
    List checked = ( List )fCheckedStateStore.get( elem );
    if( checked != null && ( !checked.isEmpty() ) ) {
      result = true;
    } else {
      // if any children of treeElement are still gray-checked then elem
      // must remain gray-checked as well
      Object[] children = getTreeChildren( elem );
      for( int i = 0; !result && i < children.length; ++i ) {
        if( fCheckedStateStore.containsKey( children[ i ] ) ) {
          result = true;
        }
      }
    }
    return result;
  }
  
  /** Returns whether the passed tree item should be white-checked. */
  protected boolean determineShouldBeWhiteChecked( final Object treeElement ) {
    return    areAllChildrenWhiteChecked( treeElement )
           && areAllElementsChecked( treeElement );
  }
  
  /**
   *  Recursively adds appropriate tree elements to the collection of
   *  known white-checked tree elements. */
  protected void determineWhiteCheckedDescendents( final Object treeElement ) {
    // always go through all children first since their white-checked
    // statuses will be needed to determine the white-checked status for
    // this tree element
    Object[] children = getTreeChildren( treeElement );
    for( int i = 0; i < children.length; ++i ) {
      determineWhiteCheckedDescendents( children[ i ] );
    }
    // now determine the white-checked status for this tree element
    if( determineShouldBeWhiteChecked( treeElement ) ) {
      setWhiteChecked( treeElement, true );
    }
  }
  /**
   * Causes the tree viewer to expand all its items
   */
  public void expandAll() {
    treeViewer.expandAll();
  }
  /**
   *  Answers a flat collection of all of the checked elements in the
   *  list portion of self
   *
   *  @return java.util.Vector
   */
  public Iterator getAllCheckedListItems() {
    Set result = new HashSet();
    Iterator listCollectionsEnum = fCheckedStateStore.values().iterator();
    while( listCollectionsEnum.hasNext() ) {
      result.addAll( ( List )listCollectionsEnum.next() );
    }
    return result.iterator();
  }
  
  /**
   *  Returns a count of the number of list items associated with a
   *  given tree item.
   */
  protected int getListItemsSize( final Object treeElement ) {
    Object[] elements = getListElements( treeElement );
    return elements.length;
  }
  
  /** Adds the given filter to the tree viewer and triggers refiltering and 
   * resorting of the elements. */
  public void addTreeFilter( final ViewerFilter filter ) {
    treeViewer.addFilter( filter );
  }
  
  /** Adds the given filter to the list viewer and triggers refiltering and 
   * resorting of the elements. */
  public void addListFilter( final ViewerFilter filter ) {
    listViewer.addFilter( filter );
  }
  
  /**
   *  Logically gray-check all ancestors of treeItem by ensuring that they
   *  appear in the checked table
   */
  protected void grayCheckHierarchy( final Object treeElement ) {
    // if this tree element is already gray then its ancestors all are as well
    if( fCheckedStateStore.containsKey( treeElement ) ) {
      return; // no need to proceed upwards from here
    }
    fCheckedStateStore.put( treeElement, new ArrayList() );
    if( determineShouldBeWhiteChecked( treeElement ) ) {
      setWhiteChecked( treeElement, true );
    }
    Object parent = fTreeContentProvider.getParent( treeElement );
    if( parent != null ) {
      grayCheckHierarchy( parent );
    }
  }
  
  /**
   *  Sets the initial checked state of the passed list element to true.
   */
  public void initialCheckListItem( final Object element ) {
    Object parent = fTreeContentProvider.getParent( element );
    fCurrentTreeSelection = parent;
    // As this is not done from the UI then set the box for updating from
    // the selection to false
    listItemChecked( element, true, false );
    updateHierarchy( parent );
  }
  
  /** Sets the initial checked state of the passed element to true, as well 
   * as to all of its children and associated list elements */
  public void initialCheckTreeItem( final Object element ) {
    treeItemChecked( element, true );
  }
  
  /**
   *  Initializes this group's viewers after they have been laid out.
   */
  protected void initialize() {
    treeViewer.setInput(fRoot);
  }
  /** Callback that's invoked when the checked status of an item in the list
   *  is changed by the user. Do not try and update the hierarchy if we are 
   *  building the initial list. */
  protected void listItemChecked( final Object listElement,
                                  final boolean state,
                                  final boolean updatingFromSelection ) {
    List checkedListItems 
      = ( List )fCheckedStateStore.get( fCurrentTreeSelection );

    if( state ) {
      if( checkedListItems == null ) {
        // since the associated tree item has gone from 0 -> 1 checked
        // list items, tree checking may need to be updated
        grayCheckHierarchy( fCurrentTreeSelection );
        checkedListItems = ( List )fCheckedStateStore
            .get( fCurrentTreeSelection );
      }
      checkedListItems.add( listElement );
    } else {
      checkedListItems.remove( listElement );
      if( checkedListItems.isEmpty() ) {
        // since the associated tree item has gone from 1 -> 0 checked
        // list items, tree checking may need to be updated
        ungrayCheckHierarchy( fCurrentTreeSelection );
      }
    }
    if( updatingFromSelection ) {
      updateHierarchy( fCurrentTreeSelection );
    }
  }
  
  /**
   *  Notifies all checked state listeners that the passed element has had
   *  its checked state changed to the passed state
   */
  protected void notifyCheckStateChangeListeners( 
                                          final CheckStateChangedEvent event ) {
    Iterator listenersEnum = fListeners.iterator();
    while( listenersEnum.hasNext() ) {
      ICheckStateListener next = ( ICheckStateListener )listenersEnum.next();
      next.checkStateChanged( event );
    }
  }
  /**
   *Sets the contents of the list viewer based upon the specified selected
   *tree element.  This also includes checking the appropriate list items.
   *
   *@param treeElement java.lang.Object
   */
  protected void populateListViewer(final Object treeElement) {
    if( treeElement != fCurrentTreeSelection ) {
      fCurrentTreeSelection = treeElement;
      listViewer.setInput( treeElement );
      List listItemsToCheck = ( List )fCheckedStateStore.get( treeElement );
  
      if( listItemsToCheck != null ) {
        Iterator listItemsEnum = listItemsToCheck.iterator();
        while( listItemsEnum.hasNext() ) {
          listViewer.setChecked( listItemsEnum.next(), true );
        }
      }
    }
  }
  
  /**
   *  Removes the passed listener from self's collection of clients
   *  that listen for changes to element checked states
   *
   *  @param listener ICheckStateListener
   */
  public void removeCheckStateListener( final ICheckStateListener listener ) {
    fListeners.remove( listener );
  }
  
  /** Handles the selection of an item in the tree viewer */
  public void selectionChanged(final SelectionChangedEvent event) {
    Runnable op = new Runnable() {
      public void run() {
        IStructuredSelection sel = ( IStructuredSelection )event.getSelection();
        Object selectedElement = sel.getFirstElement();
        if( selectedElement == null ) {
          fCurrentTreeSelection = null;
          listViewer.setInput( fCurrentTreeSelection );
        } else {
          populateListViewer( selectedElement );
        }
      }
    };
    Display display = listViewer.getTable().getShell().getDisplay();
    BusyIndicator.showWhile( display, op ); 
  }

  /** Selects or deselect all of the elements in the tree depending on the 
   * value of the selection boolean. Be sure to update the displayed files 
   * as well. */
  public void setAllSelections(final boolean selection) {
    //Potentially long operation - show a busy cursor
    BusyIndicator.showWhile( treeViewer.getControl().getDisplay(), 
                             new Runnable() {
      public void run() {
        setTreeChecked(fRoot, selection);
        listViewer.setAllChecked(selection);
      }
    });
  }

  /**
   *  Sets the sorter that is to be applied to self's list viewer
   */
  public void setListSorter( final ViewerSorter sorter ) {
    listViewer.setSorter( sorter );
  }
  
  /**
   * Sets the root of the widget to be new Root. Regenerate all of the 
   * tables and lists from this value.
   */
  public void setRoot( final Object newRoot ) {
    this.fRoot = newRoot;
    initialize();
  }
  
  /** Sets the checked state of the passed tree element, recursively */
  protected void setTreeChecked( final Object treeElement, 
                                 final boolean state ) {
    if( treeElement.equals( fCurrentTreeSelection ) ) {
      listViewer.setAllChecked( state );
    }

    if( state ) {
      Object[] listItems = getListElements( treeElement );
      List listItemsChecked = new ArrayList();
      for( int i = 0; i < listItems.length; ++i ) {
        listItemsChecked.add( listItems[ i ] );
      }
      fCheckedStateStore.put( treeElement, listItemsChecked );
    } else {
      fCheckedStateStore.remove( treeElement );
    }

    setWhiteChecked( treeElement, state );
    treeViewer.setChecked( treeElement, state );
    treeViewer.setGrayed( treeElement, false );

    // now logically check/uncheck all children as well
    Object[] children = getTreeChildren( treeElement );
    for( int i = 0; i < children.length; ++i ) {
      setTreeChecked( children[ i ], state );
    }
  }

  /** Adjusts the collection of references to white-checked tree elements 
   *  appropriately. */
  protected void setWhiteChecked( final Object treeElement, 
                                  final boolean isWhiteChecked ) {
    if (isWhiteChecked) {
      if (!fWhiteCheckedTreeItems.contains(treeElement)) {
        fWhiteCheckedTreeItems.add(treeElement);
      }
    } else {
      fWhiteCheckedTreeItems.remove(treeElement);
    }
  }

  /** Callback that's invoked when the checked status of an item in the tree
   *  is changed by the user.  */
  protected void treeItemChecked( final Object treeElement, 
                                  final boolean state ) {
    // recursively adjust all child tree elements appropriately
    setTreeChecked( treeElement, state );

    Object parent = fTreeContentProvider.getParent( treeElement );
    if( parent == null ) {
      return;
    }

    // now update upwards in the tree hierarchy
    if( state ) {
      grayCheckHierarchy( parent );
    } else {
      ungrayCheckHierarchy( parent );
    }

    updateHierarchy( treeElement );
  }
  /**
   *  Logically un-gray-check all ancestors of treeItem iff appropriate.
   */
  protected void ungrayCheckHierarchy( final Object treeElement ) {
    if( !determineShouldBeAtLeastGrayChecked( treeElement ) ) {
      fCheckedStateStore.remove( treeElement );
    }

    Object parent = fTreeContentProvider.getParent( treeElement );
    if( parent != null ) {
      ungrayCheckHierarchy( parent );
    }
  }
  
  /** Sets the checked state of self and all ancestors appropriately */
  protected void updateHierarchy( final Object treeElement ) {
    boolean whiteChecked = determineShouldBeWhiteChecked( treeElement );
    boolean shouldBeAtLeastGray 
      = determineShouldBeAtLeastGrayChecked( treeElement );

    treeViewer.setChecked( treeElement, whiteChecked || shouldBeAtLeastGray );
    setWhiteChecked( treeElement, whiteChecked );
    if( whiteChecked ) {
      treeViewer.setGrayed( treeElement, false );
    } else {
      treeViewer.setGrayed( treeElement, shouldBeAtLeastGray );
    }
    // proceed up the tree element hierarchy
    Object parent = fTreeContentProvider.getParent( treeElement );
    if( parent != null ) {
      updateHierarchy( parent );
    }
  }
  
  /**
   * Update the selections of the tree elements in items to reflect the new
   * selections provided.
   * 
   * @param items with keys of Object (the tree element) 
   *        and values of List (the selected list elements).
   */
  public void updateSelections( final Map items ) {
    // Potentially long operation - show a busy cursor
    BusyIndicator.showWhile( treeViewer.getControl().getDisplay(), 
                             new Runnable() {
      public void run() {
        handleUpdateSelection( items );
      }
    } );
  }
  
  /**
   * Returns the result of running the given elements through the filters.
   *
   * @param elements the elements to filter
   * @return only the elements which all filters accept
   */
  protected Object[] filter( final ViewerFilter[] filters,
                             final Object[] elements ) {
    Object[] result = elements;
    if( filters != null ) {
      ArrayList filtered = new ArrayList( elements.length );
      for( int i = 0; i < elements.length; i++ ) {
        boolean add = true;
        for( int j = 0; j < filters.length; j++ ) {
          add = filters[ j ].select( null, null, elements[ i ] );
          if( !add ) {
            break;
          }
        }
        if( add ) {
          filtered.add( elements[ i ] );
        }
      }
      result = filtered.toArray();
    }
    return result;
  }

  private void handleUpdateSelection( final Map items ) {
    Iterator keyIterator= items.keySet().iterator();

    // Update the store before the hierarchy to prevent updating parents 
    // before all of the children are done
    while (keyIterator.hasNext()) {
      Object key= keyIterator.next();
      // Replace the items in the checked state store with those from the 
      // supplied items
      List selections= (List) items.get(key);
      if( selections.size() == 0 ) {
        //If it is empty remove it from the list
        fCheckedStateStore.remove(key);
      } else {
        fCheckedStateStore.put(key, selections);
        // proceed up the tree element hierarchy
        Object parent= fTreeContentProvider.getParent(key);
        if (parent != null) {
          addToHierarchyToCheckedStore(parent);
        }
      }
    }

    //Now update hierarchies
    keyIterator= items.keySet().iterator();

    while (keyIterator.hasNext()) {
      Object key= keyIterator.next();
      updateHierarchy(key);
      if (fCurrentTreeSelection != null && fCurrentTreeSelection.equals(key)) {
        listViewer.setAllChecked(false);
        listViewer.setCheckedElements(((List) items.get(key)).toArray());
      }
    }
  }   
  
  public boolean isTreeItemGreyChecked( final Object object ) {
    return treeViewer.getGrayed( object );
  }

  public void expandTreeToLevel( final Object object, final int level ) {
    treeViewer.expandToLevel( object, level );
  }

  public void setTreeSelection( final ISelection selection ) {
    treeViewer.setSelection( selection );
  }
  
  public Set getWhiteCheckedTreeItems() {
    return new HashSet( fWhiteCheckedTreeItems );
  }
  
  public void addCheckStateListener( final ICheckStateListener listener ) {
    fListeners.add( listener );
  }

  public void setTreeSorter( final ViewerSorter sorter ) {
    treeViewer.setSorter( sorter );
  }

  /** Answer a collection of all of the checked elements in the tree portion
   *  of self */
  public Set getAllCheckedTreeItems() {
    return new HashSet( fCheckedStateStore.keySet() );
  }
  
  /** Answers the number of elements that have been checked by the
   *  user. */
  public int getCheckedElementCount() {
    return fCheckedStateStore.size();
  }
  
  
  // UI creation methods
  //////////////////////
  
  protected void createContents( final Composite parent, 
                                 final int width,
                                 final int height ) {
    Composite composite = new Composite( parent, SWT.NONE );
    GridLayout layout = new GridLayout();
    layout.numColumns = 2;
    layout.makeColumnsEqualWidth = true;
    layout.marginHeight = 0;
    layout.marginWidth = 0;
    composite.setLayout( layout );
    composite.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    createTreeViewer( composite, width / 2, height );
    createListViewer( composite, width / 2, height );
  }

  protected void createListViewer( final Composite parent, 
                                   final int width, 
                                   final int height ) {
    listViewer = CheckboxTableViewer.newCheckList( parent, SWT.BORDER );
    listViewer.setUseHashlookup( true );
    listViewer.getTable().setLayoutData( createGridData( width, height ) );
    listViewer.setContentProvider( fListContentProvider );
    listViewer.setLabelProvider( fListLabelProvider );
    listViewer.addCheckStateListener( this );
  }

  protected void createTreeViewer( final Composite parent, 
                                   final int width, 
                                   final int height ) {
    Tree tree = new Tree( parent, SWT.CHECK | SWT.BORDER );
    tree.setLayoutData( createGridData( width, height ) );

    treeViewer = new CheckboxTreeViewer( tree );
    treeViewer.setUseHashlookup( true );
    treeViewer.setContentProvider( fTreeContentProvider );
    treeViewer.setLabelProvider( fTreeLabelProvider );

    ITreeViewerListener tvListener = new ITreeViewerListener() {
      public void treeCollapsed( final TreeExpansionEvent event ) {
        // unused
      }

      public void treeExpanded( final TreeExpansionEvent event ) {
        Object item = event.getElement();
        // First see if the children need to be given their checked state at
        // all. If they've already been realized then this won't be necessary
        if( !fExpandedTreeNodes.contains( item ) ) {
          fExpandedTreeNodes.add( item );
          checkNewTreeElements( getTreeChildren( item ) );
        }
      }
    };
    treeViewer.addTreeListener( tvListener );
    treeViewer.addCheckStateListener( this );
    treeViewer.addSelectionChangedListener( this );
  }

  private GridData createGridData( final int width, final int height ) {
    GridData result = new GridData( GridData.FILL_BOTH );
    result.widthHint = width;
    result.heightHint = height;
    return result;
  }
  
  
  // helping methods
  //////////////////
  
  private Object[] getTreeChildren( final Object element ) {
    return filter( treeViewer.getFilters(), 
                   fTreeContentProvider.getChildren( element ) );
  }

  private Object[] getListElements( final Object element ) {
    return filter( listViewer.getFilters(), 
                   fListContentProvider.getElements( element ) );
  }
}
