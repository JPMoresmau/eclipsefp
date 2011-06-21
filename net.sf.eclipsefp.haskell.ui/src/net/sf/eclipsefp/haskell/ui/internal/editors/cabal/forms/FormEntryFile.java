package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.eclipse.ui.model.WorkbenchViewerSorter;


public class FormEntryFile extends FormEntry {

  CheckboxTreeViewer treeField;

  @Override
  public void init( final IProject project, final Composite parent,
      final FormToolkit toolkit, final int style ) {
    treeField = new CheckboxTreeViewer( parent, SWT.BORDER | SWT.FLAT );
    toolkit.adapt( treeField.getControl(), true, true );

    treeField.setLabelProvider( new WorkbenchLabelProvider() );
    treeField.setContentProvider( new LimitedWorkbenchContentProvider() );
    treeField.setSorter( new WorkbenchViewerSorter() );
    treeField.setInput( project );
  }

  @Override
  public Control getControl() {
    return treeField.getControl();
  }

  @Override
  public int heightHint() {
    return 150;
  }

  @Override
  public void setValue( final String value, final boolean blockNotification ) {
    // Do nothing
  }

  @Override
  public String getValue() {
    return "";
  }

  @Override
  public void setEditable( final boolean editable ) {
    treeField.getControl().setEnabled( editable );
  }

}
