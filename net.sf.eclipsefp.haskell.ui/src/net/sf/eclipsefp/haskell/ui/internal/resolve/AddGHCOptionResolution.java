/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.resolve;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.cabalmodel.RealValuePosition;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.graphics.Image;


/**
 * Add a ghc option to the Cabal file
 * @author JP Moresmau
 *
 */
public class AddGHCOptionResolution extends AddPackageDependency {

  public AddGHCOptionResolution( final String pkg ) {
    super( pkg );
  }

  @Override
  public String getLabel() {
    return NLS.bind( UITexts.resolve_addoption, getValue() );
  }

  @Override
  public ICompletionProposal getCompletionProposal( final IMarker marker,
      final IDocument document ) {
    return new AddPackageDependencyProposal( getValue(), marker ){
      /* (non-Javadoc)
       * @see net.sf.eclipsefp.haskell.ui.internal.resolve.AddPackageDependencyProposal#getCabalField()
       */
      @Override
      protected CabalSyntax getCabalField() {
       return CabalSyntax.FIELD_GHC_OPTIONS;
      }

      @Override
      public String getDisplayString() {
        return NLS.bind( UITexts.resolve_addoption, getValue() );
      }

      /* (non-Javadoc)
       * @see net.sf.eclipsefp.haskell.ui.internal.resolve.AddPackageDependencyProposal#update(net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza)
       */
      @Override
      protected RealValuePosition update( final PackageDescriptionStanza pds ) {
        String val=pds.getProperties().get( getCabalField() );
        if (val!=null && val.contains( getValue() )){
          return null;
        }
        if (val!=null && val.length()>0){
          val=val+" "+getValue();
        } else {
          val=getValue();
        }
        return pds.update( getCabalField(), val );
      }

      /* (non-Javadoc)
       * @see net.sf.eclipsefp.haskell.ui.internal.resolve.AddPackageDependencyProposal#getImage()
       */
      @Override
      public Image getImage() {
       return null;
      }
    };
  }


}
