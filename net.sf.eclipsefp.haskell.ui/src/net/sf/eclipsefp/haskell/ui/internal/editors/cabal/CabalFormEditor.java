// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal;

import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.pages.advanced.AdvancedPage;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.pages.overview.OverviewPage;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.forms.editor.FormEditor;

/** <p>an editor for Cabal package description files.</p>
  *
  * <p>Note: this class is declared in <code>plugin.xml</code>.</p>
  *
  * @author Leif Frenzel
  */
public class CabalFormEditor extends FormEditor {

  private CabalEditor cabalSourceEditor;
  private PackageDescription packageDescription;
  private OverviewPage overview;
  private AdvancedPage advanced;

  public PackageDescription getPackageDescription() {
    return packageDescription;
  }


  public void setPackageDescription( final PackageDescription packageDescription ) {
    this.packageDescription = packageDescription;
    cabalSourceEditor.setPackageDescription( packageDescription );
    overview.setPackageDescription( packageDescription );
    advanced.setPackageDescription( packageDescription );
  }

  public IDocument getModel() {
    IDocument result = null;
    if( cabalSourceEditor != null ) {
      result = cabalSourceEditor.getDocument();
    }
    return result;
  }


  public CabalEditor getCabalSourceEditor() {
    return cabalSourceEditor;
  }


  public OverviewPage getOverview() {
    return overview;
  }

  public AdvancedPage getAdvanced() {
    return advanced;
  }

  // interface methdods of FormEditor
  ///////////////////////////////////

  @Override
  protected void addPages() {
    try {
      overview = new OverviewPage( this );
      addPage(overview);
      advanced = new AdvancedPage( this );
      addPage(advanced);
      cabalSourceEditor = new CabalEditor(this);
      addPage( cabalSourceEditor, getEditorInput() );
      setPageText( 2, UITexts.cabalFormEditor_tabSource );
    } catch( final CoreException cex ) {
      HaskellUIPlugin.log( "Unable to create form pages.", cex ); //$NON-NLS-1$
    }
  }

  @Override
  public void doSave( final IProgressMonitor monitor ) {
    if( cabalSourceEditor != null ) {
      cabalSourceEditor.doSave( monitor );
    }
  }

  @Override
  public void doSaveAs() {
    if( cabalSourceEditor != null ) {
      cabalSourceEditor.doSaveAs();
    }
  }

  @Override
  public boolean isSaveAsAllowed() {
    boolean result = false;
    if( cabalSourceEditor != null ) {
      result = cabalSourceEditor.isSaveAsAllowed();
    }
    return result;
  }

  @Override
  public void init( final IEditorSite site,
                    final IEditorInput input ) throws PartInitException {
    super.init( site, input );
    if( input instanceof IFileEditorInput ) {
      IFileEditorInput fei = ( IFileEditorInput )input;
      setPartName( fei.getFile().getName() );
    }
  }


  // interface methods of IAdaptable
  //////////////////////////////////

  @Override
  public Object getAdapter( final Class adapter ) {
    Object result = super.getAdapter( adapter );
    if( result == null && cabalSourceEditor != null ) {
      result = cabalSourceEditor.getAdapter( adapter );
    }
    return result;
  }
}
