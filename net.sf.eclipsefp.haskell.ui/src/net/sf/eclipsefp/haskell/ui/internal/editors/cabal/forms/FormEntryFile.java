/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import java.util.HashSet;
import java.util.Set;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.eclipse.ui.views.navigator.ResourceComparator;

/**
 * Form entry for selecting a file.
 * @author Alejandro Serrano
 *
 */
public class FormEntryFile extends FormEntry implements ICheckStateListener {
  public static int FILES=1;
  public static int DIRECTORIES=2;


  CheckboxTreeViewer treeField;
  boolean ignoreModify = false;
  int typeFields;

  public FormEntryFile() {
    this(FILES | DIRECTORIES);
  }

  public FormEntryFile(final int typeFields) {
    super();
    this.typeFields = typeFields;
  }

  @Override
  public void init( final IProject project, final Composite parent,
      final FormToolkit toolkit, final int style ) {
    treeField = new CheckboxTreeViewer( parent, SWT.NULL );
    toolkit.adapt( treeField.getControl(), true, true );

    treeField.setLabelProvider( new WorkbenchLabelProvider() );
    treeField.setContentProvider( new LimitedWorkbenchContentProvider( (typeFields & FILES) ==0 ) );
    treeField.setComparator( new ResourceComparator( ResourceComparator.NAME ) );
    treeField.addCheckStateListener( this );
    treeField.setInput( project );
  }

  @Override
  public Control getControl() {
    return treeField.getControl();
  }

  @Override
  public int heightHint() {
    return 100;
  }

  @Override
  public void setValue( final String value, final boolean blockNotification ) {
    String newValue = ( value == null ) ? "" : value;
    String oldValue = this.getValue();
    if( newValue.equals( oldValue ) ) {
      return;
    }

    ignoreModify = true;
    String[] elements = newValue.split( "," );
    Set<String> ss=new HashSet<String>();

    for (String element : elements) {
      ss.add(element.trim());
    }

    setValues(ss);
    ignoreModify = false;

    /* if (!blockNotification) {
      notifyTextValueChanged();
    } */
  }

  protected void setValues(final Set<String> files){
    LimitedWorkbenchContentProvider provider = (LimitedWorkbenchContentProvider)treeField.getContentProvider();
    Set<IResource> parents=new HashSet<IResource>();
    seeChecked(provider, provider.getElements( treeField.getInput() ), files,parents);
    for (IResource parent:parents){
      manageParentState( parent );
    }
  }

  protected void seeChecked( final LimitedWorkbenchContentProvider provider,
      final Object[] objects, final Set<String> files,final Set<IResource> parents ) {
    for (Object o : objects) {
      IResource res = (IResource)o;
      if (!files.contains( res.getProjectRelativePath().toString() )) {
        // Not found
        treeField.setChecked( res, false );
      } else {
        treeField.setChecked( res, true );
        IContainer parent=res.getParent();
        if (parent instanceof IFolder){
          parents.add(parent);
        }
      }

      seeChecked(provider, provider.getChildren( o ), files,parents);
    }
  }

  @Override
  public String getValue() {
    StringBuilder builder = new StringBuilder();
    for( Object o: treeField.getCheckedElements() ) {
      IResource res = ( IResource )o;
      if ((typeFields & DIRECTORIES)==0){
        if (res instanceof IFolder){
          continue;
        }
      }
      if ((typeFields & FILES)==0){
        if (res instanceof IFile){
          continue;
        }
      }
      IPath path = res.getProjectRelativePath();

      if( builder.length() > 0 ) {
        builder.append( ", " );
      }
      builder.append( path.toString() );
    }
    return builder.toString();
  }

  @Override
  public void setEditable( final boolean editable ) {
    treeField.getControl().setEnabled( editable );
  }

  public void checkStateChanged( final CheckStateChangedEvent event ) {
    if( !ignoreModify ) {
      if (event.getElement() instanceof IFolder){
        treeField.setSubtreeChecked( event.getElement(), event.getChecked() );
      }

      notifyTextValueChanged();
    }
    LimitedWorkbenchContentProvider provider=(LimitedWorkbenchContentProvider)treeField.getContentProvider();
    Object parent=provider.getParent( event.getElement() );
    manageParentState(parent);
  }

  private void manageParentState(final Object parent){
    if (parent!=null && ((typeFields & DIRECTORIES)==0)){
      LimitedWorkbenchContentProvider provider=(LimitedWorkbenchContentProvider)treeField.getContentProvider();
      boolean gotGrayed=false;
      boolean gotUnchecked=false;
      boolean gotChecked=false;
      for (Object child:provider.getChildren( parent )){
        boolean c=treeField.getChecked( child );
        boolean g=treeField.getGrayed( child );
        if (g){
          gotGrayed=true;
        }
        if (c){
          gotChecked=true;
        } else {
          gotUnchecked=true;
        }

      }
      if (gotChecked){
        if (gotUnchecked || gotGrayed){
          treeField.setGrayChecked( parent, true );
        } else {
          treeField.setGrayed( parent, false );
          treeField.setChecked( parent, true );
        }
      } else if (gotGrayed){
        treeField.setGrayChecked( parent, true );
      } else {
        treeField.setGrayed( parent, false );
        treeField.setChecked( parent, false );
      }
    }
  }

}
