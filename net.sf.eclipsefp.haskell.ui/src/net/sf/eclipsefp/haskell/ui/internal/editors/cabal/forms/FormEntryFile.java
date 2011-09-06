/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
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

  CheckboxTreeViewer treeField;
  boolean ignoreModify = false;
  boolean onlyDirs;

  public FormEntryFile() {
    this(false);
  }

  public FormEntryFile(final boolean onlyDirs) {
    super();
    this.onlyDirs = onlyDirs;
  }

  @Override
  public void init( final IProject project, final Composite parent,
      final FormToolkit toolkit, final int style ) {
    treeField = new CheckboxTreeViewer( parent, SWT.NULL );
    toolkit.adapt( treeField.getControl(), true, true );

    treeField.setLabelProvider( new WorkbenchLabelProvider() );
    treeField.setContentProvider( new LimitedWorkbenchContentProvider( onlyDirs ) );
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
    for (String element : elements) {
      element.trim();
    }

    LimitedWorkbenchContentProvider provider = (LimitedWorkbenchContentProvider)treeField.getContentProvider();
    seeChecked(provider, provider.getElements( treeField.getInput() ), new HashSet<String>(Arrays.asList(elements)));
    ignoreModify = false;

    /* if (!blockNotification) {
      notifyTextValueChanged();
    } */
  }

  protected void seeChecked( final LimitedWorkbenchContentProvider provider,
      final Object[] objects, final Set<String> files ) {
    for (Object o : objects) {
      IResource res = (IResource)o;
      if (!files.contains( res.getProjectRelativePath().toOSString() )) {
        // Not found
        treeField.setChecked( res, false );
      } else {
        treeField.setChecked( res, true );
      }

      seeChecked(provider, provider.getChildren( o ), files);
    }
  }

  @Override
  public String getValue() {
    StringBuilder builder = new StringBuilder();
    for( Object o: treeField.getCheckedElements() ) {
      IResource res = ( IResource )o;
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
      notifyTextValueChanged();
    }
  }

}
