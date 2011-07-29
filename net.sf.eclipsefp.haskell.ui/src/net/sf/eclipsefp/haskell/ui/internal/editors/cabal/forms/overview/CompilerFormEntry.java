/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.overview;

import java.util.Vector;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.apache.tools.ant.util.StringUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.List;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;


public class CompilerFormEntry extends FormEntry {

  private List compilerList;
  private Action addAction;
  private Action removeAction;

  @Override
  public void init( final IProject project, final Composite parent,
      final FormToolkit toolkit, final int style ) {

    compilerList = new List( parent, SWT.SINGLE );
    GridData listGD = new GridData( GridData.FILL_BOTH );
    listGD.grabExcessHorizontalSpace = true;
    listGD.grabExcessVerticalSpace = true;
    listGD.verticalSpan = 2;
    compilerList.setLayoutData( listGD );
    toolkit.adapt( compilerList, true, true );
    compilerList.setData(FormToolkit.KEY_DRAW_BORDER, FormToolkit.TEXT_BORDER);

    addAction = new Action( UITexts.cabalEditor_add, IAction.AS_PUSH_BUTTON ) {

      @Override
      public void run() {
        ChooseCompilerDialog dialog = new ChooseCompilerDialog( compilerList
            .getShell() );
        if( dialog.open() == Window.OK ) {
          compilerList.add( dialog.getValue() );
          CompilerFormEntry.this.notifyTextValueChanged();
        }
      }
    };
    addAction.setImageDescriptor( PlatformUI.getWorkbench().getSharedImages()
        .getImageDescriptor( ISharedImages.IMG_OBJ_ADD ) );

    removeAction = new Action( UITexts.cabalEditor_remove,
        IAction.AS_PUSH_BUTTON ) {

      @Override
      public void run() {
        // Remove the selected elements
        if( compilerList.getSelectionIndices().length > 0 ) {
          compilerList.remove( compilerList.getSelectionIndices() );
          CompilerFormEntry.this.notifyTextValueChanged();
        }
      }
    };
    removeAction.setImageDescriptor( PlatformUI.getWorkbench()
        .getSharedImages().getImageDescriptor( ISharedImages.IMG_ELCL_REMOVE ) );
  }

  @Override
  public Control getControl() {
    return compilerList;
  }

  @Override
  public int heightHint() {
    return 120;
  }

  @Override
  public void setValue( final String value, final boolean blockNotification ) {
    String newValue = ( value == null ) ? "" : value;
    String oldValue = this.getValue();
    if( newValue.equals( oldValue ) ) {
      return;
    }

    compilerList.removeAll();
    Vector<String> elements = StringUtils.split( newValue, ',' );
    compilerList.removeAll();
    for( String element: elements ) {
      compilerList.add( element.trim() );
    }

    // If we didn't want to block notifications, notify at the end
    if( !blockNotification ) {
      notifyTextValueChanged();
    }
  }

  @Override
  public String getValue() {
    StringBuilder builder = new StringBuilder();
    for( String element: compilerList.getItems() ) {
      if( builder.length() > 0 ) {
        builder.append( ", " );
      }
      builder.append( element );
    }
    return builder.toString();
  }

  @Override
  public void setEditable( final boolean editable ) {
    compilerList.setEnabled( editable );
  }

  public Action getAddAction() {
    return addAction;
  }

  public Action getRemoveAction() {
    return removeAction;
  }

}
