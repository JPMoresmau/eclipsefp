/**
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.resolve;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.cabalmodel.RealValuePosition;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;

/**
 * sets a field in the cabal file to a specific value
 *
 * @author JP Moresmau
 *
 */
public class CabalFieldSetter extends MarkerCompletion {
  /**
   * the field to update
   */
  private final CabalSyntax field;
  /**
   * the value to set
   */
  private final String value;


  public CabalFieldSetter( final CabalSyntax field, final String value ) {
    super();
    this.field = field;
    this.value = value;
  }



  @Override
  public String getLabel() {
    return NLS.bind(UITexts.resolve_set_cabalfield,field.getCabalName(),value);
  }

  @Override
  public ICompletionProposal getCompletionProposal( final IMarker marker,
      final IDocument document ) {
    return new ICompletionProposal() {

      @Override
      public Point getSelection( final IDocument document ) {
        return null;
      }

      @Override
      public Image getImage() {
        return null;
      }

      @Override
      public String getDisplayString() {
        return getLabel();
      }

      @Override
      public IContextInformation getContextInformation() {
        return null;
      }

      @Override
      public String getAdditionalProposalInfo() {
        return null;
      }

      @Override
      public void apply( final IDocument document ) {
        PackageDescription pd=PackageDescriptionLoader.load( document.get() );
        // use package stanza for now
        RealValuePosition rvp=pd.getPackageStanza().update( field, value );
        if (rvp!=null){
          rvp.updateDocument( document );
        }
      }
    };

  }

}
