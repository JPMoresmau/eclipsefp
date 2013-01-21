package net.sf.eclipsefp.haskell.ghccompiler.core;

import java.util.List;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.ICabalContributor;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;
import net.sf.eclipsefp.haskell.core.compiler.HsImplementationType;
import net.sf.eclipsefp.haskell.core.compiler.IHsImplementation;

/**
 * <p>Add GHC options to Cabal executable and library sections</p>
  *
  * @author JP Moresmau
 */
public class GhcCabalContributor implements ICabalContributor {

  @Override
  public void contributeOnNewProject( final PackageDescription pd ) {
    IHsImplementation impl=CompilerManager.getCurrentHsImplementation();
    if (impl!=null && impl.getType().equals( HsImplementationType.GHC )){
      List<String> ls=new CompilerParams().construct(impl.getVersion());
      StringBuilder sbOptions=new StringBuilder();
      StringBuilder sbExtensions=new StringBuilder();
      for (String s:ls){
        if (s!=null){
          if (s.startsWith( "-X" )){ //$NON-NLS-1$
            if (sbExtensions.length()>0){
              sbExtensions.append(" "); //$NON-NLS-1$
            }
            sbExtensions.append(s.substring( 2 ));
          } else {
            if (sbOptions.length()>0){
              sbOptions.append(" "); //$NON-NLS-1$
            }

            sbOptions.append(s);
          }
        }

      }
      String valOptions=sbOptions.toString();
      String valExtensions=sbExtensions.toString();
      for (PackageDescriptionStanza pds:pd.getStanzas()){
        if(CabalSyntax.SECTION_EXECUTABLE.equals( pds.getType() ) || CabalSyntax.SECTION_LIBRARY.equals( pds.getType() ) || CabalSyntax.SECTION_TESTSUITE.equals( pds.getType() )){
          if (valOptions.length()>0){
            pds.update( CabalSyntax.FIELD_GHC_OPTIONS, valOptions );
          }
          if (valExtensions.length()>0){
            pds.update( CabalSyntax.FIELD_EXTENSIONS, valExtensions );
          }
        }
      }
    }
  }

}
