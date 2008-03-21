package net.sf.eclipsefp.haskell.core.project.util;

import java.util.HashMap;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

public class PreferencesStub implements Preferences {

	private final HashMap<String, String> fTable = new HashMap<String, String>();

	public String absolutePath() {return "";}

	public String[] childrenNames() throws BackingStoreException {return null;}

	public void clear() throws BackingStoreException {}

	public void flush() throws BackingStoreException {}

	public String get(final String key, final String def) {
		String result = fTable.get(key);
		if (null == result) {
      result = def;
    }
		return result;
	}

	public boolean getBoolean(final String key, final boolean def) { return false; }

	public byte[] getByteArray(final String key, final byte[] def) { return null; }

	public double getDouble(final String key, final double def) { return 0; }

	public float getFloat(final String key, final float def) { return 0; }

	public int getInt(final String key, final int def) { return 0; }

	public long getLong(final String key, final long def) { return 0; }

	public String[] keys() throws BackingStoreException { return null; }

	public String name() { return null; }

	public Preferences node(final String pathName) { return null; }

	public boolean nodeExists(final String pathName) throws BackingStoreException {
		return false;
	}

	public Preferences parent() { return null; }

	public void put(final String key, final String value) {
		fTable.put(key, value);
	}

	public void putBoolean(final String key, final boolean value) {}

	public void putByteArray(final String key, final byte[] value) {}

	public void putDouble(final String key, final double value) {}

	public void putFloat(final String key, final float value) {}

	public void putInt(final String key, final int value) {}

	public void putLong(final String key, final long value) {}

	public void remove(final String key) {}

	public void removeNode() throws BackingStoreException {}

	public void sync() throws BackingStoreException {}

}
