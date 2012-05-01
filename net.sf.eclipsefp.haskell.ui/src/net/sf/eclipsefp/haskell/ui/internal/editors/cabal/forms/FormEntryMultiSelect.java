/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import java.util.HashSet;
import java.util.Set;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;

/**
 * Form entry that shows a list with attached checkboxes,
 * where the user can select zero or more of them.
 * @author Alejandro Serrano
 *
 */
public class FormEntryMultiSelect extends FormEntry implements
    ICheckStateListener {

  ITreeContentProvider contents;
  CheckboxTreeViewer treeField;
  boolean ignoreModify = false;
  boolean onlyOneSelected = false;

  public FormEntryMultiSelect( final ITreeContentProvider contents ) {
    this(contents, false);
  }

  public FormEntryMultiSelect( final ITreeContentProvider contents, final boolean onlyOneSelected ) {
    this.contents = contents;
    this.onlyOneSelected = onlyOneSelected;
  }

  @Override
  public void init( final IProject project, final Composite parent,
      final FormToolkit toolkit, final int style ) {
    treeField = new CheckboxTreeViewer( parent, SWT.NONE );
    toolkit.adapt( treeField.getControl(), true, true );

    treeField.setContentProvider( contents );
    treeField.setComparator( new ViewerComparator() );
    treeField.addCheckStateListener( this );
    treeField.setInput( project );

  }

  @Override
  public Control getControl() {
    return this.treeField.getControl();
  }

  public CheckboxTreeViewer getTree() {
    return this.treeField;
  }

  @Override
  public int heightHint() {
    return 200;
  }

  @Override
  public void setValue( final String value, final boolean blockNotification ) {
    String newValue = ( value == null ) ? "" : value;
    String oldValue = this.getValue();
    if( newValue.equals( oldValue ) ) {
      return;
    }

    ignoreModify = true;
    String[] elements = newValue.split( "(,|\\s)" );
    Set<String> set=new HashSet<String>();
    for( String element: elements ) {
      set.add(element.trim());
    }
    for( Object o: contents.getElements( null ) ) {
      String s = ( String )o;
      if( !set.contains( s ) ) {
        treeField.setChecked( s, false );
      } else {
        treeField.setChecked( s, true );
      }
    }
    ignoreModify = false;

    if( !blockNotification ) {
      notifyTextValueChanged();
    }
  }

  @Override
  public String getValue() {
    StringBuilder builder = new StringBuilder();
    for( Object o: treeField.getCheckedElements() ) {
      String s = ( String )o;
      if( builder.length() > 0 ) {
        builder.append( ", "+PlatformUtil.NL );
      }
      builder.append( s );
    }
    return builder.toString();
  }

  public String getNonSelectedValue() {
    StringBuilder builder = new StringBuilder();
    for( Object o: contents.getElements( null ) ) {
      if( !treeField.getChecked( o ) ) {
        String s = ( String )o;
        if( builder.length() > 0 ) {
          builder.append( ", "+PlatformUtil.NL );
        }
        builder.append( s );
      }
    }
    return builder.toString();
  }

  @Override
  public void setEditable( final boolean editable ) {
    this.treeField.getControl().setEnabled( editable );
  }

  @Override
  public void checkStateChanged( final CheckStateChangedEvent event ) {
    if (onlyOneSelected) {
      boolean previousIgnoreModify = ignoreModify;
      ignoreModify = true;
      for (Object o : treeField.getCheckedElements()) {
        if (!event.getElement().equals( o )) {
          treeField.setChecked( o, false );
        }
      }
      ignoreModify = previousIgnoreModify;
    }
    if( !ignoreModify ) {
      notifyTextValueChanged();
    }
  }

}
