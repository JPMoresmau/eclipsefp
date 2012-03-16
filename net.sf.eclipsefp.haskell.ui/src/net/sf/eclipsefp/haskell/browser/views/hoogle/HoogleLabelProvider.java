/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.hoogle;

import java.util.ArrayList;
import java.util.Map;
import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.HoogleResult;
import net.sf.eclipsefp.haskell.browser.items.HoogleResultConstructor;
import net.sf.eclipsefp.haskell.browser.items.HoogleResultDeclaration;
import net.sf.eclipsefp.haskell.browser.util.ImageCache;
import net.sf.eclipsefp.haskell.browser.views.NoDatabaseRoot;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;

/**
 * Label provider for Hoogle results.
 * @author Alejandro Serrano
 *
 */
public class HoogleLabelProvider implements ILabelProvider {

  @Override
  @SuppressWarnings ( "unchecked" )
  public Image getImage( final Object element ) {
    if (element instanceof NoDatabaseRoot){
      return ImageCache.DATABASE;

    }
    HoogleResult result = null;
    if (element instanceof HoogleResult) {
      result = (HoogleResult)element;
    } else {
      Map.Entry<String, Object> entry = (Map.Entry<String, Object>)element;
      if (entry.getValue() instanceof ArrayList) {
        result = ( (ArrayList<HoogleResult>)entry.getValue() ).get( 0 );
      } else {
        // If we came here, it means that we are in an element defined in several places
        return ImageCache.MODULE_CONTENT;
      }
    }

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
      case KEYWORD:
        return ImageCache.KEYWORD;
    }

    return null;
  }

  @Override
  @SuppressWarnings ( "unchecked" )
  public String getText( final Object element ) {

    HoogleResult result = null;
    if (element instanceof NoDatabaseRoot){
      return UITexts.scionBrowserNoDatabaseLoadedOrHoogleNotPresent;

    } else if (element instanceof HoogleResult) {
      result = (HoogleResult)element;
      switch(result.getType()) {
        case PACKAGE:
        case MODULE:
        case KEYWORD:
          return result.getName();
        case DECLARATION:
          Declaration decl = ((HoogleResultDeclaration)result).getDeclaration();
          return decl.getShownName();
        case CONSTRUCTOR:
          HoogleResultConstructor con = (HoogleResultConstructor)result;
          return con.getConstructor().getShownName();
      }
    } else {
      Map.Entry<String, Object> entry = (Map.Entry<String, Object>)element;
      if (entry.getValue() instanceof ArrayList) {
        // We are in the root of an element defined in several places
        return entry.getKey();
      } else {
        // We are in an instance of an element defined several times,
        // so we need to show the name of the module
        result = (HoogleResult)entry.getValue();
        switch(result.getType()) {
          case PACKAGE:
          case MODULE:
          case KEYWORD:
            return null; // This should not happen
          case DECLARATION:
            HoogleResultDeclaration decl = (HoogleResultDeclaration)result;
            return decl.getModule();
          case CONSTRUCTOR:
            HoogleResultConstructor con = (HoogleResultConstructor)result;
            return con.getModule();
        }
      }
    }



    return null;
  }

  // Listeners: not used
  @Override
  public void addListener( final ILabelProviderListener listener ) {
    // Do nothing
  }

  @Override
  public void dispose() {
    // Do nothing
  }

  @Override
  public boolean isLabelProperty( final Object element, final String property ) {
    // Do nothing
    return false;
  }

  @Override
  public void removeListener( final ILabelProviderListener listener ) {
    // Do nothing
  }
}
