package net.sf.eclipsefp.haskell.core.test;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;

/**
 * Scope for preferences used in unit tests.
 * Based on the code of {@link InstanceScope}.
 *
 * @author Thomas ten Cate
 */
public class TestScope implements IScopeContext {

  /**
   * String constant (value of <code>"instance"</code>) used for the
   * scope name for the instance preference scope.
   */
  public static final String SCOPE = "test"; //$NON-NLS-1$

  @Override
  public IPath getLocation() {
    // Return null. InstanceScope does this too, and remarks:
    // The instance location usually corresponds to the state
    // location of the bundle and we don't know what bundle we are dealing with.
    return null;
  }

  @Override
  public String getName() {
    return SCOPE;
  }

  @Override
  public IEclipsePreferences getNode( final String qualifier ) {
    if (qualifier == null) {
      throw new IllegalArgumentException();
    }
    return (IEclipsePreferences) Platform.getPreferencesService().getRootNode().node(getName()).node(qualifier);
  }

}
