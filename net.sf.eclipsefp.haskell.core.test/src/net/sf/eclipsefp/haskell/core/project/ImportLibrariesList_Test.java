package net.sf.eclipsefp.haskell.core.project;

import static org.easymock.EasyMock.createNiceMock;
import java.io.File;
import junit.framework.TestCase;
import net.sf.eclipsefp.haskell.core.project.util.NullProjectPropertiesEvent;
import net.sf.eclipsefp.haskell.core.project.util.PreferencesStub;
import org.eclipse.core.resources.IProject;
import org.osgi.service.prefs.Preferences;

public class ImportLibrariesList_Test extends TestCase {

	public void testSavesToOsgiPreferences() {
		TestingLibrariesList list = new TestingLibrariesList();
		list.add(list.createLibrary("/usr/lib", true));
		list.save();
		Preferences prefs = list.getPreferences();
		assertEquals("/usr/lib,t".replace('/', File.separatorChar),
				prefs.get("PROJECT_IMPORT_LIBRARIES", ""));
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
		if (fPreferences == null) {
      fPreferences = new PreferencesStub();
    }
		return fPreferences;
	}

}
