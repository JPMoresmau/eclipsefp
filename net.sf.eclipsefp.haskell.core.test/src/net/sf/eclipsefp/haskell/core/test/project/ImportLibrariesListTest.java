package net.sf.eclipsefp.haskell.core.test.project;

import org.eclipse.core.resources.IProject;
import org.osgi.service.prefs.Preferences;

import junit.framework.TestCase;
import static org.easymock.EasyMock.*;

import net.sf.eclipsefp.haskell.core.project.IProjectPropertiesEvent;
import net.sf.eclipsefp.haskell.core.project.ImportLibrariesList;
import net.sf.eclipsefp.haskell.core.test.project.util.NullProjectPropertiesEvent;
import net.sf.eclipsefp.haskell.core.test.project.util.PreferencesStub;

public class ImportLibrariesListTest extends TestCase {
	
	public void testSavesToOsgiPreferences() {
		TestingLibrariesList list = new TestingLibrariesList();
		list.add(list.createLibrary("/usr/lib", true));
		list.save();
		Preferences prefs = list.getPreferences();
		assertEquals("/usr/lib,t", prefs.get("PROJECT_IMPORT_LIBRARIES", ""));
	}

}

class TestingLibrariesList extends ImportLibrariesList {

	private Preferences fPreferences;

	public TestingLibrariesList() {
		super(createNiceMock(IProject.class));
	}
	
	@Override protected Preferences createPrefs() {
		return getPreferences();
	}
	
	@Override protected IProjectPropertiesEvent createProjectPropertiesEvent() {
		return new NullProjectPropertiesEvent();
	}

	@Override protected String haskellCorePluginId() {
		return "testing.plugin";
	}

	public Preferences getPreferences() {
		if (fPreferences == null) fPreferences = new PreferencesStub();
		return fPreferences;
	}
	
}
