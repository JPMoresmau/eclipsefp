// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.internal.ui.wizards;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

/** <p>A wizard page that is shown when no configurator pages are registered
  * in this Eclipse installation.</p>
  *
  * @author Leif Frenzel
  */
class NoPagesInfoPage extends WizardPage {

  
  NoPagesInfoPage() {
    super( NoPagesInfoPage.class.getName() );
    setTitle( "Nothing to configure" );
  }
  
  // interface methods of WizardPage
  //////////////////////////////////
  
  public void createControl( final Composite parent ) {
    Label label = new Label( parent, SWT.WRAP );
    label.setText( getInfoText() );
    setControl( label );
  }
  
  
  // helping methods
  //////////////////
  
  private String getInfoText() {
    return   "This wizard is used to help you configure commands that are "
           + "needed to be\nknown to some plugins in your Eclipse "
           + "installation.\n\nThere are, however, currently no plugins "
           + "that make use of this functionality.";
  }
}
