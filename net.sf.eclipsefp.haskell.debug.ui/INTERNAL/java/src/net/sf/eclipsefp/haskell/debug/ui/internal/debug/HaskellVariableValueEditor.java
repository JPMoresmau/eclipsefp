package net.sf.eclipsefp.haskell.debug.ui.internal.debug;


import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.debug.ui.internal.util.UITexts;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IVariable;
import org.eclipse.debug.ui.actions.IVariableValueEditor;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * This editor only forces evaluation when clicking on OK
 * @author JP Moresmau
 *
 */
public class HaskellVariableValueEditor implements IVariableValueEditor {


  public boolean editVariable( final IVariable variable, final Shell shell ) {

    Dialog d=new Dialog(shell) {
      @Override
      protected Control createDialogArea( final Composite parent ) {
        Composite c=(Composite)super.createDialogArea( parent );
        Text l=new Text( c, SWT.MULTI | SWT.WRAP);
        l.setEditable( false );
        l.setLayoutData(new GridData(GridData.FILL_BOTH));
        l.setText( UITexts.debug_editor_text );
        return c;
      }

      @Override
      protected void configureShell( final Shell newShell ) {
        super.configureShell( newShell );
        newShell.setText( UITexts.debug_editor_title );
        newShell.setSize( 300, 200 );
      }
    };
    int code=d.open();
    if (code==Window.OK){
      try {
        variable.setValue( "force" ); //$NON-NLS-1$
      } catch (DebugException de){
        HaskellCorePlugin.log( de );
      }
    }
    return true;
  }

  public boolean saveVariable( final IVariable variable, final String expression,
      final Shell shell ) {
    return false;
  }

}
