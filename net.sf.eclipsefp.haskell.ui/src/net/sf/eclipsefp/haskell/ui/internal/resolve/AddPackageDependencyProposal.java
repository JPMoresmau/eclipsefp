package net.sf.eclipsefp.haskell.ui.internal.resolve;

import net.sf.eclipsefp.haskell.browser.util.ImageCache;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.cabalmodel.RealValuePosition;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.ui.texteditor.IDocumentProvider;


public class AddPackageDependencyProposal implements ICompletionProposal {

  private final String pkg;
  private final IMarker marker;

  public AddPackageDependencyProposal(final String pkg, final IMarker marker) {
    this.pkg = pkg;
    this.marker = marker;
  }

  public void apply( final IDocument document ) {
    IProject p=marker.getResource().getProject();
    IFile f=ScionInstance.getCabalFile( p );
    IDocumentProvider prov=new TextFileDocumentProvider();
    try {
      prov.connect( f );
      IDocument doc=prov.getDocument( f );
      PackageDescription pd=PackageDescriptionLoader.load( f );
      int length=pd.getStanzas().size();
      for (int a=0;a<length;a++){
        PackageDescriptionStanza pds=pd.getStanzas().get(a);
        CabalSyntax cs=pds.getType();
        if (CabalSyntax.SECTION_EXECUTABLE.equals(cs) || CabalSyntax.SECTION_LIBRARY.equals(cs) || CabalSyntax.SECTION_TESTSUITE.equals(cs)){
          RealValuePosition rvp=pds.addToPropertyList( CabalSyntax.FIELD_BUILD_DEPENDS, pkg );
          if (rvp!=null){
            rvp.updateDocument( doc );
            pd=PackageDescriptionLoader.load( doc.get() );
          }
        }
      }
      prov.saveDocument( new NullProgressMonitor(), f, doc, true );
    } catch (CoreException ce){
      HaskellUIPlugin.log( ce );

    }
  }

  public Point getSelection( final IDocument document ) {
    return null;
  }

  public String getAdditionalProposalInfo() {
    return null;
  }

  public String getDisplayString() {
    return NLS.bind( UITexts.resolve_addpackage, pkg );
  }

  public Image getImage() {
    return ImageCache.PACKAGE;
  }

  public IContextInformation getContextInformation() {
    return null;
  }

}
