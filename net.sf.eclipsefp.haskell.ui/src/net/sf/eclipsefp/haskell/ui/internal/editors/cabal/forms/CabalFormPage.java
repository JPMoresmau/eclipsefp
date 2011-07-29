package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.forms.editor.FormPage;


public abstract class CabalFormPage extends FormPage {

  PackageDescription packageDescription = null;
  boolean isLoaded = false;
  protected IProject project;

  public CabalFormPage( final FormEditor editor, final String id,
      final String title, final IProject project ) {
    super( editor, id, title );
    this.project = project;
  }

  protected boolean isLoaded() {
    return this.isLoaded;
  }

  protected void finishedLoading() {
    if (packageDescription != null) {
      setPackageDescriptionInternal( packageDescription );
    }
    this.isLoaded = true;
  }

  protected abstract void setPackageDescriptionInternal(
      final PackageDescription packageDescription );

  public void setPackageDescription( final PackageDescription packageDescription ) {
    this.packageDescription = packageDescription;
    if( isLoaded() ) {
      setPackageDescriptionInternal( packageDescription );
    }
  }

  protected PackageDescription getPackageDescription() {
    return this.packageDescription;
  }

  protected GridLayout createGridLayout( final int cols, final int sideMargin,
      final int topMargin ) {
    GridLayout layout = new GridLayout( cols, true );
    layout.marginHeight = 0;
    layout.marginWidth = 0;

    layout.marginTop = topMargin;
    layout.marginBottom = topMargin;
    layout.marginLeft = sideMargin;
    layout.marginRight = sideMargin;

    layout.horizontalSpacing = 8; // 15;
    layout.verticalSpacing = 8; // 12;
    return layout;
  }

  protected GridLayout createUnequalGridLayout( final int cols, final int sideMargin,
      final int topMargin ) {
    GridLayout layout = new GridLayout( cols, false );
    layout.marginHeight = 0;
    layout.marginWidth = 0;

    layout.marginTop = topMargin;
    layout.marginBottom = topMargin;
    layout.marginLeft = sideMargin;
    layout.marginRight = sideMargin;

    layout.horizontalSpacing = 20;
    layout.verticalSpacing = 17;
    return layout;
  }
}
