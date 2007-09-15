// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.common;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

import net.sf.eclipsefp.haskell.core.halamo.*;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;

/** <p>A label provider for Haskell language elements in views.</p>
  * 
  * @author Leif Frenzel
  */
public class HaskellLabelProvider extends LabelProvider implements IImageNames {

  @Override
  public String getText( final Object element ) {
    String result;
    if( element instanceof IImport ) {
      result = ( ( IImport )element ).getImportedElement();
    } else if( element instanceof ExportGroup ) {
      result = "export specifications";
    } else if( element instanceof ImportGroup ) {
      result = "import declarations";
    } else if( element instanceof IFunctionBinding ) {
      result = getFunctionBindingText( ( IFunctionBinding )element );
    } else if( element instanceof IInfixDeclaration ) {
      result = getInfixDeclText( ( IInfixDeclaration )element );
    } else if( element instanceof ITypeSignature ) {
      result = getTypeSignatureText( ( ITypeSignature )element );
    } else if( element instanceof IHaskellLanguageElement ) {
      result = ( ( IHaskellLanguageElement )element ).getName();
    } else {
      // use the inherited (toString()) variant
      result = super.getText( element );
    }
    return result;
  }
  
  @Override
  public Image getImage( final Object element ) {
    Image result = null;
    if( element instanceof IModule ) {
      result = HaskellUIImages.getImage( MODULE );
    } else if( element instanceof ExportGroup ) {
      result = HaskellUIImages.getImage( EXPORT_GROUP );
    } else if( element instanceof IExportModuleContent ) {
      result = HaskellUIImages.getImage( EXPORT_MODULE_CONTENT );
    } else if( element instanceof IExportSpecification ) {
      result = HaskellUIImages.getImage( EXPORT_SPECIFICATION );
    } else if( element instanceof ImportGroup ) {
      result = HaskellUIImages.getImage( IMPORT_GROUP );
    } else if( element instanceof IImport ) {
      result = HaskellUIImages.getImage( IMPORT );
    } else if( element instanceof IDataDeclaration ) {
      result = HaskellUIImages.getImage( DATA_DECL );
    } else if( element instanceof IConstructor ) {
      result = HaskellUIImages.getImage( HS_NAME );
    } else if( element instanceof IPatternBinding ) {
      result = HaskellUIImages.getImage( PATTERN_BINDING );
    } else if( element instanceof IFunctionBinding ) {
      result = HaskellUIImages.getImage( FUNCTION_BINDING );
    } else if( element instanceof IMatch ) {
      result = HaskellUIImages.getImage( HS_NAME );
    } else if( element instanceof IClassDeclaration ) {
      result = HaskellUIImages.getImage( CLASS_DECL );
    } else if( element instanceof IInstanceDeclaration ) {
      result = HaskellUIImages.getImage( INSTANCE_DECL );
    } else if( element instanceof IDefaultDeclaration ) {
      result = HaskellUIImages.getImage( DEFAULT_DECL );
    } else if( element instanceof IInfixDeclaration ) {
      result = getInfixImage( ( IInfixDeclaration )element );
    } else if( element instanceof ITypeSignature ) {
      result = HaskellUIImages.getImage( TYPE_SIGNATURE );
    } else if( element instanceof ITypeDeclaration ) {
      result = HaskellUIImages.getImage( TYPE_DECL );
    } else if( element instanceof INewTypeDeclaration ) {
      result = HaskellUIImages.getImage( NEWTYPE_DECL );
    } else if( element instanceof String ) {
      result = HaskellUIImages.getImage( HS_NAME );
    } else {
      result = getDefaultImage();
    }
    return result;
  }


  // helping methods
  //////////////////
  
  private Image getDefaultImage() {
    String imageKey = ISharedImages.IMG_OBJ_ELEMENT;
    return PlatformUI.getWorkbench().getSharedImages().getImage( imageKey );
  }
  
  private String getTypeSignatureText( final ITypeSignature signature ) {
    String result = signature.getName();
    // policy for type signatures: if only one identifier is bound in the
    // type signature, display that identifier directly
    if( signature.getIdentifiers().length == 1 ) {
      result = signature.getIdentifiers()[ 0 ];
    } 
    return result;
  }

  private String getInfixDeclText( final IInfixDeclaration decl ) {
    String result = decl.getName();
    // policy for type signatures: if only one identifier is bound in the
    // type signature, display that identifier directly
    if( decl.getOperators().length == 1 ) {
      result = decl.getOperators()[ 0 ];
    } 
    return result;
  }

  private String getFunctionBindingText( final IFunctionBinding funBind ) {
    String result = funBind.getName();
    // policy for function bindings: if only one identifier is bound, display 
    // it directly
    if( funBind.getMatches().length == 1 ) {
      result = funBind.getMatches()[ 0 ].getName();
    } 
    return result;
  }
  private Image getInfixImage( final IInfixDeclaration declaration ) {
    Image result = null;
    switch( declaration.getAssociativity() ) {
      case IInfixDeclaration.ASSOC_NONE:
        result = HaskellUIImages.getImage( INFIXNONE_DECL );
        break;
      case IInfixDeclaration.ASSOC_LEFT:
        result = HaskellUIImages.getImage( INFIXL_DECL );
        break;
      case IInfixDeclaration.ASSOC_RIGHT:
        result = HaskellUIImages.getImage( INFIXR_DECL );
        break;
    }
    return result;
  }
}