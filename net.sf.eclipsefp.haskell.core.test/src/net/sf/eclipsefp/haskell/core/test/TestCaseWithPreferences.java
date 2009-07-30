package net.sf.eclipsefp.haskell.core.test;

import java.util.ArrayList;
import junit.framework.TestCase;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;

/**
 * Test case that uses preferences.
 * It adds a specialized scope for them, and changes the default lookup order.
 *
 * @author Thomas ten Cate
 */
public class TestCaseWithPreferences extends TestCase {

  private final ArrayList<String> fQualifiers = new ArrayList<String>();
  private IScopeContext fPrefsScope;

  public TestCaseWithPreferences() {
    addQualifier( HaskellCorePlugin.getPluginId() );
  }

  /**
   * Adds a plug-in to the list of plug-ins that should use the test scope for lookups.
   * This only has an effect before setUp() is called.
   * Normally, this should be called from the constructor.
   *
   * @param qualifier the plug-in ID
   */
  protected void addQualifier(final String qualifier) {
    fQualifiers.add(qualifier);
  }

  protected IScopeContext getPrefsScope() {
    return fPrefsScope;
  }

  public IEclipsePreferences getCorePrefs() {
    return getPrefsScope().getNode( HaskellCorePlugin.getPluginId() );
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    fPrefsScope = new TestScope();
    // add TestScope to the preferences lookup order
    String[] lookupOrder = new String[] {
        TestScope.SCOPE, ProjectScope.SCOPE, InstanceScope.SCOPE, ConfigurationScope.SCOPE, DefaultScope.SCOPE
    };
    for (String qualifier : fQualifiers) {
      Platform.getPreferencesService().setDefaultLookupOrder( qualifier, null, lookupOrder );
    }
  }

  @Override
  protected void tearDown() throws Exception {
    for (String qualifier : fQualifiers) {
      Platform.getPreferencesService().setDefaultLookupOrder( qualifier, null, null );
    }
    getPrefsScope().getNode( "" ).removeNode();
    super.tearDown();
  }

}
