package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import net.sf.eclipsefp.haskell.browser.util.ImageCache;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;


public class DependenciesDialogLabelProvider implements ILabelProvider {

  public Image getImage( final Object element ) {
    return ImageCache.PACKAGE;
  }

  public String getText( final Object element ) {
    HaskellPackage pkg = (HaskellPackage)element;
    return pkg.getIdentifier().getName();
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
