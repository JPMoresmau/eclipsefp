package net.sf.eclipsefp.haskell.core.project.util;

import java.util.HashMap;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

public class PreferencesStub implements Preferences {

	private final HashMap<String, String> fTable = new HashMap<>();

	@Override
  public String absolutePath() {return "";}

	@Override
  public String[] childrenNames() throws BackingStoreException {return null;}

	@Override
  public void clear() throws BackingStoreException {}

	@Override
  public void flush() throws BackingStoreException {}

	@Override
  public String get(final String key, final String def) {
		String result = fTable.get(key);
		if (null == result) {
      result = def;
    }
		return result;
	}

	@Override
  public boolean getBoolean(final String key, final boolean def) { return false; }

	@Override
  public byte[] getByteArray(final String key, final byte[] def) { return null; }

	@Override
  public double getDouble(final String key, final double def) { return 0; }

	@Override
  public float getFloat(final String key, final float def) { return 0; }

	@Override
  public int getInt(final String key, final int def) { return 0; }

	@Override
  public long getLong(final String key, final long def) { return 0; }

	@Override
  public String[] keys() throws BackingStoreException { return null; }

	@Override
  public String name() { return null; }

	@Override
  public Preferences node(final String pathName) { return null; }

	@Override
  public boolean nodeExists(final String pathName) throws BackingStoreException {
		return false;
	}

	@Override
  public Preferences parent() { return null; }

	@Override
  public void put(final String key, final String value) {
		fTable.put(key, value);
	}

	@Override
  public void putBoolean(final String key, final boolean value) {}

	@Override
  public void putByteArray(final String key, final byte[] value) {}

	@Override
  public void putDouble(final String key, final double value) {}

	@Override
  public void putFloat(final String key, final float value) {}

	@Override
  public void putInt(final String key, final int value) {}

	@Override
  public void putLong(final String key, final long value) {}

	@Override
  public void remove(final String key) {}

	@Override
  public void removeNode() throws BackingStoreException {}

	@Override
  public void sync() throws BackingStoreException {}

}
