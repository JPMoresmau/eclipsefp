// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.outline;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

/** <p>label provider for the Cabal editor's outline page.</p>
  *
  * @author Leif Frenzel
  */
public class CabalOutlineLP extends LabelProvider {

  @Override
  public Image getImage( final Object element ) {
    /*Image result = super.getImage( element );

    if( element instanceof ExecutableStanza ) {
      result = HaskellUIImages.getImage( IImageNames.EXECUTABLE_STANZA );
    } else if( element instanceof LibraryStanza ) {
      result = HaskellUIImages.getImage( IImageNames.LIBRARY_STANZA );
    } else if( element instanceof GeneralStanza ) {
      result = HaskellUIImages.getImage( IImageNames.GENERAL_STANZA );
    }*/
    Image result=null;
    if (element instanceof PackageDescriptionStanza){
      PackageDescriptionStanza pds=(PackageDescriptionStanza)element;
      if(CabalSyntax.SECTION_EXECUTABLE.equals( pds.getType() )){
        result = HaskellUIImages.getImage( IImageNames.EXECUTABLE_STANZA );
      } else if (CabalSyntax.SECTION_LIBRARY.equals( pds.getType() )){
        result = HaskellUIImages.getImage( IImageNames.LIBRARY_STANZA );
      } else if (CabalSyntax.SECTION_IF.equals( pds.getType() )){
        result = HaskellUIImages.getImage( IImageNames.IF_STANZA );
      } else if (CabalSyntax.SECTION_ELSE.equals( pds.getType() )){
        result = HaskellUIImages.getImage( IImageNames.ELSE_STANZA );
      } else if (CabalSyntax.SECTION_FLAG.equals( pds.getType() )){
        result = HaskellUIImages.getImage( IImageNames.FLAG_STANZA );
      } else if (CabalSyntax.SECTION_TESTSUITE.equals( pds.getType() )){
        result = HaskellUIImages.getImage( IImageNames.TESTSUITE_STANZA );
      } else if (CabalSyntax.SECTION_SOURCE_REPOSITORY.equals( pds.getType() )){
        result = HaskellUIImages.getImage( IImageNames.SOURCEREP_STANZA );
      } else if (pds.getType()==null){
        result = HaskellUIImages.getImage( IImageNames.GENERAL_STANZA );
      }
    }
    if (result==null){
      result = super.getImage( element );
    }

    return result;
  }
}
