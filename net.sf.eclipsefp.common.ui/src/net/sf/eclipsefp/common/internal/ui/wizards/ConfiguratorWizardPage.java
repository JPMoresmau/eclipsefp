// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.internal.ui.wizards;

import java.lang.reflect.InvocationTargetException;

import net.sf.eclipsefp.common.ui.configurator.IConfiguratorPage;
import net.sf.eclipsefp.common.ui.configurator.IProbe;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/** <p>implements a wizard page with the specialized settings for the 
  * configrator wizard.</p>
  * 
  * @author Leif Frenzel 
  */
class ConfiguratorWizardPage extends WizardPage {

  private final IConfiguratorPage configuratorPage;
  private final IProbe probe;

  ConfiguratorWizardPage( final String pageName, 
                          final IConfiguratorPage cfgPage, 
                          final IProbe probe ) {
    super( pageName );
    this.configuratorPage = cfgPage;
    this.probe = probe;
  }

  IConfiguratorPage getConfiguratorPage() {
    return configuratorPage;
  }
  
  // interface methods of WizardPage
  //////////////////////////////////
  
  public void createControl( final Composite parent ) {
    if( probe != null ) {
      setControl( wrapProbeUI( parent, configuratorPage, probe ) );
    } else {
      setControl( configuratorPage.createControl( parent ) );
    }
  }
  
  
  // UI creation methods
  //////////////////////

  private Control wrapProbeUI( final Composite parent,
                               final IConfiguratorPage cfgPage, 
                               final IProbe probe ) {
    Composite result = new Composite( parent, SWT.NONE );
    result.setLayout( new GridLayout( 1, false ) );

    Control pageControl = cfgPage.createControl( result );
    pageControl.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    Composite probeComposite = createProbeComposite( result );

    ToolBar tb = new ToolBar( probeComposite, SWT.FLAT );
    ToolItem item = new ToolItem( tb, SWT.FLAT );
    item.addSelectionListener( new SelectionAdapter() {
      public void widgetSelected( final SelectionEvent evt ) {
        runOperation( createOp( probe ) );
      }
    } );
    item.setImage( getProbeImage() );
    Label label = new Label( probeComposite, SWT.NONE );
    String msg =   " Try to determine settings automatically " 
                 + "(may take some time)";
    label.setText( msg );
    return result;
  }

  private Composite createProbeComposite( final Composite result ) {
    Composite probeComposite = new Composite( result, SWT.NONE );
    GridLayout gridLayout = new GridLayout( 2, false );
    gridLayout.horizontalSpacing = 0;
    probeComposite.setLayout( gridLayout );
    GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    gridData.verticalAlignment = GridData.VERTICAL_ALIGN_END;
    probeComposite.setLayoutData( gridData );
    return probeComposite;
  }

  private static Image getProbeImage() {
    String key = ISharedImages.IMG_OBJS_INFO_TSK;
    return PlatformUI.getWorkbench().getSharedImages().getImage( key );
  }

  
  // helping methods
  //////////////////
  
  private void runOperation( final IRunnableWithProgress op ) {
    try {
      getWizard().getContainer().run( false, true, op );
    } catch( InvocationTargetException itex ) {
      // TODO
      itex.printStackTrace();
    } catch( InterruptedException iex ) {
      // TODO Auto-generated catch block
      iex.printStackTrace();
    }
  }
  
  private IRunnableWithProgress createOp( final IProbe probe ) {
    return new IRunnableWithProgress() {
      public void run( final IProgressMonitor monitor )
                      throws InvocationTargetException, InterruptedException {
        Object result = probe.execute( monitor );
        configuratorPage.probed( result );
      }
    };
  }
}