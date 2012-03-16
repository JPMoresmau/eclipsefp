package net.sf.eclipsefp.haskell.browser.views;

import net.sf.eclipsefp.haskell.browser.util.ImageCache;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;

/**
 * Provider used when the database or Hoogle haven't been loaded yet.
 * @author Alejandro Serrano
 *
 */
public class NoDatabaseLabelProvider implements ILabelProvider {

  boolean isHoogle;

  public NoDatabaseLabelProvider(final boolean isHoogle) {
    this.isHoogle = isHoogle;
  }

  @Override
  public Image getImage( final Object element ) {
    return ImageCache.DATABASE;
  }

  @Override
  public String getText( final Object element ) {
    return isHoogle ? UITexts.scionBrowserNoDatabaseLoadedOrHoogleNotPresent :
      UITexts.scionBrowserNoDatabaseLoaded;
  }

  @Override
  public void addListener( final ILabelProviderListener listener ) {
    // Do nothing
  }

  @Override
  public void removeListener( final ILabelProviderListener listener ) {
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

}
