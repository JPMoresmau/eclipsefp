package net.sf.eclipsefp.haskell.core.test.project.util;

import java.util.HashMap;

import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

public class PreferencesStub implements Preferences {

	private HashMap<String, String> fTable = new HashMap<String, String>();

	public String absolutePath() {return "";}

	public String[] childrenNames() throws BackingStoreException {return null;}

	public void clear() throws BackingStoreException {}

	public void flush() throws BackingStoreException {}

	public String get(String key, String def) {
		String result = fTable.get(key);
		if (null == result) result = def;
		return result;
	}

	public boolean getBoolean(String key, boolean def) { return false; }

	public byte[] getByteArray(String key, byte[] def) { return null; }

	public double getDouble(String key, double def) { return 0; }

	public float getFloat(String key, float def) { return 0; }

	public int getInt(String key, int def) { return 0; }

	public long getLong(String key, long def) { return 0; }

	public String[] keys() throws BackingStoreException { return null; }

	public String name() { return null; }

	public Preferences node(String pathName) { return null; }

	public boolean nodeExists(String pathName) throws BackingStoreException {
		return false;
	}

	public Preferences parent() { return null; }

	public void put(String key, String value) {
		fTable.put(key, value);
	}

	public void putBoolean(String key, boolean value) {}

	public void putByteArray(String key, byte[] value) {}

	public void putDouble(String key, double value) {}

	public void putFloat(String key, float value) {}

	public void putInt(String key, int value) {}

	public void putLong(String key, long value) {}

	public void remove(String key) {}

	public void removeNode() throws BackingStoreException {}

	public void sync() throws BackingStoreException {}

}
