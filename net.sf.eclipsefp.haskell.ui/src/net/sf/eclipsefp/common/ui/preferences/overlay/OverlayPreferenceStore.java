// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.preferences.overlay;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;

/** <p>A preference store that can be used for caching preference values
  * (e.g. on tabbed pages).</p>
  *
  * @author Leif Frenzel
  */
public class OverlayPreferenceStore implements IPreferenceStore {

  /** The parent preference store. */
  private final IPreferenceStore parent;
  /** The underlying preference store. */
  private final IPreferenceStore store;
  /** The keys of this store. */
  private final List<OverlayKey> overlayKeys;
  private IPropertyChangeListener propertyListener;


  public OverlayPreferenceStore( final IPreferenceStore parent ) {
    this.parent = parent;
    overlayKeys = new ArrayList<>();
    store = new PreferenceStore();
  }

  public void addKey( final OverlayType type, final String key ) {
    this.overlayKeys.add( new OverlayKey( type, key ) );
  }

  public void addBooleanKey( final String key ) {
    addKey( OverlayType.BOOLEAN, key );
  }

  public void addDoubleKey( final String key ) {
    addKey( OverlayType.DOUBLE, key );
  }

  public void addFloatKey( final String key ) {
    addKey( OverlayType.FLOAT, key );
  }

  public void addIntKey( final String key ) {
    addKey( OverlayType.INT, key );
  }

  public void addLongKey( final String key ) {
    addKey( OverlayType.LONG, key );
  }

  public void addStringKey( final String key ) {
    addKey( OverlayType.STRING, key );
  }

  /**
   * Propagates all overlay keys from this store to the parent store.
   */
  public void propagate() {
    for( Iterator<OverlayKey> i = overlayKeys.iterator(); i.hasNext(); ) {
      propagateProperty( store, i.next(), parent );
    }
  }

  public void load() {
    Iterator<OverlayKey> iter = overlayKeys.iterator();
    while( iter.hasNext() ) {
      OverlayKey key = iter.next();
      Loader.loadProperty( parent, key, store, true );
    }
  }

  public void loadDefaults() {
    Iterator<OverlayKey> iter = overlayKeys.iterator();
    while( iter.hasNext() ) {
      OverlayKey key = iter.next();
      setToDefault( key.getKey() );
    }
  }

  /**
   * Starts to listen for changes.
   */
  public void startListening() {
    if( propertyListener == null ) {
      propertyListener = new IPropertyChangeListener() {
        @Override
        public void propertyChange( final PropertyChangeEvent event ) {
          OverlayKey key = findOverlayKey( event.getProperty() );
          if( key != null ) {
            propagateProperty( parent, key, store );
          }
        }
      };
      parent.addPropertyChangeListener( propertyListener );
    }
  }

  public void stopListening() {
    if( propertyListener != null ) {
      parent.removePropertyChangeListener( propertyListener );
      propertyListener = null;
    }
  }

  // IPreferenceStore Implementation -----------------------------------------

  @Override
  public void addPropertyChangeListener( final IPropertyChangeListener li ) {
    store.addPropertyChangeListener( li );
  }

  @Override
  public void removePropertyChangeListener( final IPropertyChangeListener li ) {
    store.removePropertyChangeListener( li );
  }

  @Override
  public void firePropertyChangeEvent( final String name,
                                       final Object oldValue,
                                       final Object newValue ) {
    store.firePropertyChangeEvent( name, oldValue, newValue );
  }

  @Override
  public boolean contains( final String name ) {
    return store.contains( name );
  }

  @Override
  public boolean getBoolean( final String name ) {
    return store.getBoolean( name );
  }

  @Override
  public boolean getDefaultBoolean( final String name ) {
    return store.getDefaultBoolean( name );
  }

  @Override
  public double getDefaultDouble( final String name ) {
    return store.getDefaultDouble( name );
  }

  @Override
  public float getDefaultFloat( final String name ) {
    return store.getDefaultFloat( name );
  }

  @Override
  public int getDefaultInt( final String name ) {
    return store.getDefaultInt( name );
  }

  @Override
  public long getDefaultLong( final String name ) {
    return store.getDefaultLong( name );
  }

  @Override
  public String getDefaultString( final String name ) {
    return store.getDefaultString( name );
  }

  @Override
  public double getDouble( final String name ) {
    return store.getDouble( name );
  }

  @Override
  public float getFloat( final String name ) {
    return store.getFloat( name );
  }

  @Override
  public int getInt( final String name ) {
    return store.getInt( name );
  }

  @Override
  public long getLong( final String name ) {
    return store.getLong( name );
  }

  @Override
  public String getString( final String name ) {
    return store.getString( name );
  }

  @Override
  public boolean isDefault( final String name ) {
    return store.isDefault( name );
  }

  @Override
  public boolean needsSaving() {
    return store.needsSaving();
  }

  @Override
  public void putValue( final String name, final String value ) {
    if( covers( name ) ) {
      store.putValue( name, value );
    }
  }

  @Override
  public void setDefault( final String name, final double value ) {
    if( covers( name ) ) {
      store.setDefault( name, value );
    }
  }

  @Override
  public void setDefault( final String name, final float value ) {
    if( covers( name ) ) {
      store.setDefault( name, value );
    }
  }

  @Override
  public void setDefault( final String name, final int value ) {
    if( covers( name ) ) {
      store.setDefault( name, value );
    }
  }

  @Override
  public void setDefault( final String name, final long value ) {
    if( covers( name ) ) {
      store.setDefault( name, value );
    }
  }

  @Override
  public void setDefault( final String name, final String value ) {
    if( covers( name ) ) {
      store.setDefault( name, value );
    }
  }

  @Override
  public void setDefault( final String name, final boolean value ) {
    if( covers( name ) ) {
      store.setDefault( name, value );
    }
  }

  @Override
  public void setToDefault( final String name ) {
    store.setToDefault( name );
  }

  @Override
  public void setValue( final String name, final double value ) {
    if( covers( name ) ) {
      store.setValue( name, value );
    }
  }

  @Override
  public void setValue( final String name, final float value ) {
    if( covers( name ) ) {
      store.setValue( name, value );
    }
  }

  @Override
  public void setValue( final String name, final int value ) {
    if( covers( name ) ) {
      store.setValue( name, value );
    }
  }

  @Override
  public void setValue( final String name, final long value ) {
    if( covers( name ) ) {
      store.setValue( name, value );
    }
  }

  @Override
  public void setValue( final String name, final String value ) {
    if( covers( name ) ) {
      store.setValue( name, value );
    }
  }

  @Override
  public void setValue( final String name, final boolean value ) {
    if( covers( name ) ) {
      store.setValue( name, value );
    }
  }

  /**
   * Tries to find and return the overlay key for the given preference key
   * string.
   *
   * @param key
   *          the preference key string
   * @return the overlay key or <code>null</code> if none can be found
   */
  protected OverlayKey findOverlayKey( final String key ) {
    OverlayKey result = null;
    Iterator<OverlayKey> iter = overlayKeys.iterator();
    while( iter.hasNext() ) {
      OverlayKey overlayKey = iter.next();
      if( overlayKey.getKey().equals( key ) ) {
        result = overlayKey;
      }
    }
    return result;
  }

  protected final void propagateProperty( final IPreferenceStore origin,
                                          final OverlayKey key,
                                          final IPreferenceStore target ) {
    if( origin.isDefault( key.getKey() ) ) {
      if( !target.isDefault( key.getKey() ) ) {
        target.setToDefault( key.getKey() );
      }
    } else {
      Propagator.propagate( origin, key, target );
    }
  }

  private boolean covers( final String key ) {
    return ( findOverlayKey( key ) != null );
  }
}