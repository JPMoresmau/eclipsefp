// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.internal.ui.wizards;

import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;



/** <p>The wizard that contains all configurator pages declared as 
  * extensions.</p>
  *
  * @author Leif Frenzel
  */
public class ConfiguratorWizard extends Wizard {

  private IWizardPage[] pages;

  public ConfiguratorWizard() {
    setWindowTitle( "Command configurator" );
    setNeedsProgressMonitor( true );
  }
  
  
  // interface methods of Wizard
  //////////////////////////////
  
  public void addPages() {
    pages = PageLoader.createPages();
    if( pages.length == 0 ) {
      addPage( new NoPagesInfoPage() );
    } else {
      for( int i = 0; i < pages.length; i++ ) {
        addPage( pages[ i ] );
      }
    }
  }
  
  public boolean performFinish() {
    for( int i = 0; i < pages.length; i++ ) {
      ConfiguratorWizardPage wizardPage = ( ConfiguratorWizardPage )pages[ i ];
      wizardPage.getConfiguratorPage().performFinish();
    }
    return true;
  }
}
