package net.sf.eclipsefp.haskell.browser.views.hoogle;

import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.HoogleResult;
import net.sf.eclipsefp.haskell.browser.items.HoogleResultConstructor;
import net.sf.eclipsefp.haskell.browser.items.HoogleResultDeclaration;
import net.sf.eclipsefp.haskell.browser.util.ImageCache;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;


public class HoogleLabelProvider implements ILabelProvider {

  public Image getImage( final Object element ) {
    HoogleResult result = (HoogleResult)element;
    switch(result.getType()) {
      case PACKAGE:
        return ImageCache.PACKAGE;
      case MODULE:
        return ImageCache.MODULE;
      case CONSTRUCTOR:
        return ImageCache.CONSTRUCTOR;
      case DECLARATION:
        Declaration decl = ((HoogleResultDeclaration)result).getDeclaration();
        return ImageCache.getImageForDeclaration( decl.getType() );
    }

    return null;
  }

  public String getText( final Object element ) {
    HoogleResult result = (HoogleResult)element;
    switch(result.getType()) {
      case PACKAGE:
      case MODULE:
        return result.getName();
      case DECLARATION:
        Declaration decl = ((HoogleResultDeclaration)result).getDeclaration();
        return decl.getShownName();
      case CONSTRUCTOR:
        HoogleResultConstructor con = (HoogleResultConstructor)result;
        return con.getConstructor().getShownName();
    }

    return null;
  }

  // Listeners: not used
  public void addListener( final ILabelProviderListener listener ) {
    // Do nothing
  }

  public void dispose() {
    // Do nothing
  }

  public boolean isLabelProperty( final Object element, final String property ) {
    // Do nothing
    return false;
  }

  public void removeListener( final ILabelProviderListener listener ) {
    // Do nothing
  }
}
