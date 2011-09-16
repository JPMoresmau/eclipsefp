// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.properties;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.sf.eclipsefp.haskell.buildwrapper.types.CabalPackage;
import net.sf.eclipsefp.haskell.buildwrapper.types.Component;
import net.sf.eclipsefp.haskell.ui.properties.viewerpart.TablePart;
import net.sf.eclipsefp.haskell.ui.util.SWTUtil;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;


/** <p>special viewer part with buttons for the libraries
  * preference page.</p>
  *
  * @author Leif Frenzel
  */
class ImportLibrariesViewerPart extends TablePart {
  private final Collection<CabalPackage> allPackages= new ArrayList<CabalPackage>();
  private List<Component> components;

  public ImportLibrariesViewerPart() {
    super( new String[] { "Add...",
                          "Remove",
                          null,
                          null,
                          "Select All",
                          "Deselect All" });
  }

  private final Map<String,CabalPackage> added=new HashMap<String, CabalPackage>();
  private final Set<String> removed=new HashSet<String>();


  public void setAllPackages( final Collection<CabalPackage[]> allPackages ) {
    this.allPackages.clear();
    for (CabalPackage[] cps:allPackages){
      this.allPackages.addAll( Arrays.asList( cps ) );
    }
  }


  public void setComponents( final List<Component> components ) {
    this.components = components;
  }

  @Override
  protected void buttonSelected( final Button button, final int index ) {
    switch( index ) {
      case 0:
        handleAdd();
        break;
      case 1:
        handleDelete();
        break;
      case 4:
        selectAll( true );
        break;
      case 5:
        selectAll( false );
        break;
    }
  }

  @Override
  protected Button createButton( final Composite parent,
                                 final String label,
                                 final int index ) {
    Button button = super.createButton( parent, label, index );
    SWTUtil.setButtonDimensionHint( button );
    return button;
  }

  @Override
  protected void createMainLabel( final Composite parent, final int span ) {
    Label label = new Label( parent, SWT.NULL );
    GridData gd = new GridData( GridData.FILL );
    gd.horizontalSpan = span;
    label.setLayoutData( gd );
  }

  @Override
  protected void selectionChanged( final IStructuredSelection selection ) {
    setButtonEnabled( 1, !selection.isEmpty() );
  }



  private void selectAll( final boolean selected ) {
    if (selected){
      getTableViewer().getTable().selectAll();
    } else {
      getTableViewer().getTable().deselectAll();
    }
    setButtonEnabled( 1, selected );
  }

  private void handleAdd() {
    Collection<CabalPackage> addablePackages=new ArrayList<CabalPackage>(allPackages.size());
    for (CabalPackage cp:allPackages){
      if (!added.containsKey( cp.getName()) && (cp.getComponents().length==0 || removed.contains( cp.getName() )) ){
        addablePackages.add(cp);
      }
    }
    ImportLibrariesAddDialog addD=new ImportLibrariesAddDialog( getTableViewer().getTable().getShell(), addablePackages,components );
    if (addD.open()==Window.OK){
      CabalPackage cp=addD.getCabalPackage();
      if (cp!=null){
        added.put( cp.getName(), cp );
        getTableViewer().add( cp );
      }
    }
  }


  public Map<String, CabalPackage> getAdded() {
    return added;
  }


  public Set<String> getRemoved() {
    return removed;
  }

  private void handleDelete() {
    ISelection sel = getTableViewer().getSelection();
    IStructuredSelection selection = ( IStructuredSelection )sel;

    for (Iterator<?> it=selection.iterator();it.hasNext();){
      CabalPackage cp=(CabalPackage)it.next();
      getTableViewer().remove( cp );
      added.remove( cp.getName() );
      removed.add( cp.getName() );
    }
   //
  }
}