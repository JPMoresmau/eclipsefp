// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.project;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectPropertiesEvent;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.QualifiedName;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

/**
 * <p>
 * Encapsulates a list of import libraries that can be read from and saved to
 * the core plugin preferences.
 * </p>
 *
 * @author Leif Frenzel
 */
@Deprecated
public class ImportLibrariesList {

	private static final String KEY = "PROJECT_IMPORT_LIBRARIES"; //$NON-NLS-1$

	private final List<IImportLibrary> importLibList;
	private final IProject project;
	private IImportLibrary[] lastPersisted;

	/** constructs an import libraries list for use in the global workspace. */
	public ImportLibrariesList(final IProject project) {
		this.project = project;
		importLibList = new ArrayList<IImportLibrary>();
		load();
	}

	/**
	 * <p>
	 * saves the content of this ImportLibrariesList to the core preferences.
	 * </p>
	 */
	public void save() {
		try {
			IProjectPropertiesEvent event = createProjectPropertiesEvent();
			Preferences prefs = createPrefs();
			prefs.put(KEY, encodeImportLibraries());
			prefs.flush();
			HaskellProjectManager.broadcast(event);
			lastPersisted = getAll();
		} catch (final BackingStoreException basox) {
			String msg = "Could not store import libraries for project " //$NON-NLS-1$
					     + project.getName();
			HaskellCorePlugin.log(msg, basox);
		}
	}

	public IImportLibrary[] getAll() {
		IImportLibrary[] result = new IImportLibrary[importLibList.size()];
		importLibList.toArray(result);
		return result;
	}

	/**
	 * <p>
	 * adds the specified library to this list.
	 * </p>
	 */
	public void add(final IImportLibrary library) {
		importLibList.add(library);
	}

	/**
	 * <p>
	 * tests whether this list contains the specified library.
	 * </p>
	 */
	public boolean contains(final IImportLibrary library) {
		boolean result = false;
		for (int i = 0; !result && i < importLibList.size(); i++) {
			IImportLibrary lib = importLibList.get(i);
			result = lib.equals(library);
		}
		return result;
	}

	/**
	 * <p>
	 * removes the specified library from this list (if it was contained).
	 * </p>
	 */
	public void remove(final IImportLibrary library) {
		importLibList.remove(library);
	}

	public IImportLibrary createLibrary(final String path, final boolean used) {
		return new ImportLibrary(new Path(path), used);
	}

	// helping methods
	// ////////////////

	private void parseImportLibs(final String text) {
		String markedText = text.replace(File.pathSeparatorChar, ';');
		StringTokenizer stok = new StringTokenizer(markedText, ";"); //$NON-NLS-1$
		while (stok.hasMoreTokens()) {
			String token = stok.nextToken();
			ImportLibrary impLib = parse(token.trim());
			if (impLib != null) {
				importLibList.add(impLib);
			}
		}
	}

	private ImportLibrary parse(final String text) {
		int commaIndex = text.lastIndexOf(',');
		boolean enabled = text.charAt(commaIndex + 1) == 't';
		String path = text.substring(0, commaIndex);
		return new ImportLibrary(new Path(path), enabled);
	}

	private String encodeImportLibraries() {
		StringBuffer buf = new StringBuffer();
		IImportLibrary[] libs = getAll();
		for (int i = 0; i < libs.length; i++) {
			if (i > 0) {
				buf.append(File.pathSeparatorChar);
			}
			buf.append(encodeImportLibrary(libs[i]));
		}
		return buf.toString();
	}

	private String encodeImportLibrary(final IImportLibrary lib) {
		return lib.getPath().toOSString() + "," + (lib.isUsed() ? "t" : "f");   //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
	}

	private void load() {
		String property = createPrefs().get(KEY, ""); //$NON-NLS-1$
		if (property != null && property.length() > 0) {
			parseImportLibs(property);
		} else {
			// load prefs from legacy mechanism - we had in the past stored it
			// to
			// project properties in the workspace store, and users may still
			// have
			// something there which should be preserved
			String legacyProp = loadFromLegacy();
			if (legacyProp != null && legacyProp.length() > 0) {
				parseImportLibs(legacyProp);
				// save them to store for new mechanism
				save();
				// clear the legacy store
				clearLegacyStore();
			}
		}
		this.lastPersisted = getAll();
	}

	private void clearLegacyStore() {
		try {
			project.setPersistentProperty(getQName(), null);
		} catch (final CoreException cex) {
			// suppress; we don't want to store there anymore anyway
		}
	}

	private String loadFromLegacy() {
		String result = null;
		try {
			result = project.getPersistentProperty(getQName());
		} catch (CoreException cex) {
			String msg = "Could not read import libraries property for " //$NON-NLS-1$
					     + project;
			HaskellCorePlugin.log(msg, cex);
		}
		return result;
	}

	private QualifiedName getQName() {
		return new QualifiedName(haskellCorePluginId(), KEY);
	}

	protected String haskellCorePluginId() {
		return HaskellCorePlugin.getPluginId();
	}

	protected IProjectPropertiesEvent createProjectPropertiesEvent() {
		IHaskellProject hsProject = HaskellProjectManager.get(project);
		String id = IHaskellProject.PROPERTY_IMPORT_LIBRARIES;
		ProjectPropertiesEvent event = new ProjectPropertiesEvent(hsProject, id);
		event.setOldValue(lastPersisted);
		event.setNewValue(getAll());
		return event;
	}

	protected Preferences createPrefs() {
		return new ProjectScope(project).getNode(haskellCorePluginId());
	}
}
