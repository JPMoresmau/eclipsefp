/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.wizards;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectModelFilesOp;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.internal.wizards.NewProjectWizardPage;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.INewWizard;


/**
 * GTK project wizard: executable referencing gtk, Main.hs have default hello world
 * @author JP Moresmau
 *
 */
public class NewGtkProjectWizard extends NewHaskellProjectWizard implements INewWizard {

  /**
   *
   */
  public NewGtkProjectWizard() {
    super();
    op=new GktProjectModelFilesOp();
    getOperation().setExtraOperation( op );
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.ui.internal.wizards.ProjectCreationWizard#addPages()
   */
  @Override
  public void addPages() {
    page = new NewProjectWizardPage(){
      /* (non-Javadoc)
       * @see net.sf.eclipsefp.haskell.ui.internal.wizards.NewProjectWizardPage#createControls(org.eclipse.swt.widgets.Composite)
       */
      @Override
      protected void createControls( final Composite composite ) {
        createNameControl(composite);
        createLocationControl(composite);
      }
    };
    page.setTitle( getPageTitle() );
    page.setDescription( getPageDescription() );
    addPage( page );
  }

  @Override
  public boolean performFinish() {
    op.setExecutable( true);
    op.setLibrary( false );
    return super.performFinish();
  }

  @Override
  protected String getPageDescription() {
    return UITexts.newGtkProjectWizard_pageDesc;
  }

  @Override
  protected String getPageTitle() {
    return UITexts.newGtkProjectWizard_pageTitle;
  }

  @Override
  protected String getTheWindowTitle() {
    return UITexts.newGtkProjectWizard_windowTitle;
  }

  /**
   * override main.hs and cabal file with gtk specifics
   * @author JP Moresmau
   *
   */
  private static class GktProjectModelFilesOp extends ProjectModelFilesOp{
    /* (non-Javadoc)
     * @see net.sf.eclipsefp.haskell.core.internal.project.ProjectModelFilesOp#getCabalFile(java.lang.String)
     */
    @Override
    protected PackageDescription getCabalFile( final String name ) {
      PackageDescription pd=super.getCabalFile( name );
      PackageDescriptionStanza pds=pd.getStanzas().get( 1 );
      pds.addToPropertyList( CabalSyntax.FIELD_BUILD_DEPENDS, "gtk" ); //$NON-NLS-1$

      return pd;
    }

    /* (non-Javadoc)
     * @see net.sf.eclipsefp.haskell.core.internal.project.ProjectModelFilesOp#getMainFileContent()
     */
    @Override
    protected String getMainFileContent() {
      return "module Main where"+PlatformUtil.NL+PlatformUtil.NL+
        "import Graphics.UI.Gtk"+PlatformUtil.NL+PlatformUtil.NL+
        "main :: IO ()"+PlatformUtil.NL+
        "main = do"+PlatformUtil.NL+
        "  initGUI"+PlatformUtil.NL+
        "  window <- windowNew"+PlatformUtil.NL+
        "  button <- buttonNew"+PlatformUtil.NL+
        "  set window [ containerBorderWidth := 10,"+PlatformUtil.NL+
        "               containerChild := button ]"+PlatformUtil.NL+
        "  set button [ buttonLabel := \"Hello World\" ]"+PlatformUtil.NL+
        "  onClicked button (putStrLn \"Hello World\")"+PlatformUtil.NL+
        "  onDestroy window mainQuit"+PlatformUtil.NL+
        "  widgetShowAll window"+PlatformUtil.NL+
        "  mainGUI"+PlatformUtil.NL;
    }
  }
}
