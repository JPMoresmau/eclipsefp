/**
 *
 */
package net.sf.eclipsefp.haskell.ui.internal.resolve;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import net.sf.eclipsefp.haskell.browser.util.ImageCache;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageHelper;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageVersion;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.cabalmodel.RealValuePosition;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.ui.texteditor.IDocumentProvider;

/**
 * Add a package to the build-depends of cabal stanzas
 * @author JP Moresmau
 *
 */
public class AddPackageDependencyProposal implements ICompletionProposal {

  private String value;
  private final IMarker marker;

  public AddPackageDependencyProposal(final String value, final IMarker marker) {
    this.value = value;
    this.marker = marker;
  }

  public AddPackageDependencyProposal(final String value) {
    this.value = value;
    this.marker = null;
  }


  @Override
  public void apply( final IDocument document ) {
    apply((IFile)marker.getResource());
  }

  public void apply(final IFile res) {
    IFile f=BuildWrapperPlugin.getCabalFile( res.getProject() );
    IDocumentProvider prov=new TextFileDocumentProvider();
    try {
      prov.connect( f );
      IDocument doc=prov.getDocument( f );
      PackageDescription pd=PackageDescriptionLoader.load( f );
      String fullVersion=getValue();
      String v=CabalPackageHelper.getInstance().getLastInstalledVersion( getValue());
      if (v!=null){
        fullVersion=getValue()+ " "+ CabalPackageVersion.getMajorRangeFromMinor( v );
      }

      // find applicable stanzas if any, to not modify all of the cabal stanzas
      Set<PackageDescriptionStanza> apps=ResourceUtil.getApplicableStanzas( new IFile[]{res} );
      Set<String> appNames=new HashSet<String>();
      for (PackageDescriptionStanza pds:apps){
        Collection<String> pkgs=pds.getDependentPackages();
        // some stanzas may already have the dependency
        if(!pkgs.contains( getValue() )){
          for (String s:pkgs){
            if (s.startsWith( getValue()+" " )){
              continue;
            }
          }
          appNames.add(pds.toTypeName());
        }
      }
      value=fullVersion;
      int length=pd.getStanzas().size();
      for (int a=0;a<length;a++){
        PackageDescriptionStanza pds=pd.getStanzas().get(a);
        CabalSyntax cs=pds.getType();
        if (appNames.isEmpty() || appNames.contains( pds.toTypeName() )){
          if (CabalSyntax.SECTION_EXECUTABLE.equals(cs) || CabalSyntax.SECTION_LIBRARY.equals(cs) || CabalSyntax.SECTION_TESTSUITE.equals(cs)){
            RealValuePosition rvp=update(pds);
            if (rvp!=null){
              rvp.updateDocument( doc );
              pd=PackageDescriptionLoader.load( doc.get() );
            }
          }
        }
      }
      prov.saveDocument( new NullProgressMonitor(), f, doc, true );
    } catch (Exception ce){
      HaskellUIPlugin.log( ce );

    }
  }


  protected RealValuePosition update(final PackageDescriptionStanza pds){
    return pds.addToPropertyList( getCabalField(), getValue() );
  }

  protected CabalSyntax getCabalField(){
    return CabalSyntax.FIELD_BUILD_DEPENDS;
  }


  /**
   * @return the value
   */
  public String getValue() {
    return value;
  }

  @Override
  public Point getSelection( final IDocument document ) {
    return null;
  }

  @Override
  public String getAdditionalProposalInfo() {
    return null;
  }

  @Override
  public String getDisplayString() {
    return NLS.bind( UITexts.resolve_addpackage, value );
  }

  @Override
  public Image getImage() {
    return ImageCache.PACKAGE;
  }

  @Override
  public IContextInformation getContextInformation() {
    return null;
  }

}
