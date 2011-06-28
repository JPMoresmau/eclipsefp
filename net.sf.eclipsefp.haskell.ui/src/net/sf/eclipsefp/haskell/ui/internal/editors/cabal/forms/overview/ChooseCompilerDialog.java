/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.overview;

import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;


public class ChooseCompilerDialog extends Dialog {

  List compilerList;
  Text compilerVersion;
  String value = null;

  static final String[] compilers = { "GHC", "NHC", "YHC", "Hugs", "HBC", "Helium", "JHC", "LHC", "UHC" };

  protected ChooseCompilerDialog( final IShellProvider provider ) {
    super( provider );
  }

  protected ChooseCompilerDialog( final Shell parentShell ) {
    super( parentShell );
  }

  public String getValue() {
    return this.value;
  }

  @Override
  protected Control createDialogArea( final Composite parent ) {
    // Set window title
    parent.getShell().setText( UITexts.compilerChooser_title );
    // Create the inside
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayoutData( new GridData(GridData.FILL_BOTH) );
    // The layout
    GridLayout layout = new GridLayout();
    layout.numColumns = 2;
    composite.setLayout( layout );
    // Add compiler list
    compilerList = new List( composite, SWT.SINGLE | SWT.BORDER );
    compilerList.setItems( compilers );
    compilerList.select( 0 );
    GridData compilerGD = new GridData( GridData.FILL_BOTH );
    compilerGD.horizontalSpan = 2;
    compilerGD.grabExcessHorizontalSpace = true;
    compilerGD.grabExcessVerticalSpace = true;
    compilerList.setLayoutData( compilerGD );
    // Add version text
    Label label = new Label( composite, SWT.NONE );
    label.setText( UITexts.compilerChooser_version );
    compilerVersion = new Text( composite, SWT.SINGLE | SWT.BORDER );
    compilerVersion.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );
    return composite;
  }

  @Override
  protected void okPressed() {
    value = compilerList.getSelection()[0] + "==" + compilerVersion.getText();
    super.okPressed();
  }
}
