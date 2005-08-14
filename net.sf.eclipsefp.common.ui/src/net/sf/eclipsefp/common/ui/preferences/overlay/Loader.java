// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.preferences.overlay;

import org.eclipse.jface.preference.IPreferenceStore;


/** <p>helper for the overlay store.</p>
  * 
  * @author Leif Frenzel
  */
class Loader {

  static void loadProperty( final IPreferenceStore store, 
                            final OverlayKey key,
                            final IPreferenceStore target, 
                            final boolean forceInitialization ) {
    if( OverlayType.BOOLEAN == key.getType() ) {
      setBooleanValue( store, key, target, forceInitialization );
    } else if( OverlayType.DOUBLE == key.getType() ) {
      setDoubleValue( store, key, target, forceInitialization );
    } else if( OverlayType.FLOAT == key.getType() ) {
      setFloatValue( store, key, target, forceInitialization );
    } else if( OverlayType.INT == key.getType() ) {
      setIntValue( store, key, target, forceInitialization );
    } else if( OverlayType.LONG == key.getType() ) {
      setLongValue( store, key, target, forceInitialization );
    } else if( OverlayType.STRING == key.getType() ) {
      setStringValue( store, key, target, forceInitialization );
    }
  }

  
  // helping methods
  //////////////////
  
  private static void setStringValue( final IPreferenceStore store, 
                                      final OverlayKey key, 
                                      final IPreferenceStore target, 
                                      final boolean forceInitialization ) {
    if( forceInitialization ) {
      target.setValue( key.getKey(), "1" );
    }
    target.setValue( key.getKey(), store.getString( key.getKey() ) );
    target.setDefault( key.getKey(), store.getDefaultString( key.getKey() ) );
  }

  private static void setLongValue( final IPreferenceStore store, 
                                    final OverlayKey key, 
                                    final IPreferenceStore target, 
                                    final boolean forceInitialization ) {
    if( forceInitialization ) {
      target.setValue( key.getKey(), 1L );
    }
    target.setValue( key.getKey(), store.getLong( key.getKey() ) );
    target.setDefault( key.getKey(), store.getDefaultLong( key.getKey() ) );
  }

  private static void setIntValue( final IPreferenceStore store, 
                                   final OverlayKey key, 
                                   final IPreferenceStore target, 
                                   final boolean forceInitialization ) {
    if( forceInitialization ) {
      target.setValue( key.getKey(), 1 );
    }
    target.setValue( key.getKey(), store.getInt( key.getKey() ) );
    target.setDefault( key.getKey(), store.getDefaultInt( key.getKey() ) );
  }

  private static void setFloatValue( final IPreferenceStore orig, 
                                     final OverlayKey key, 
                                     final IPreferenceStore target, 
                                     final boolean forceInitialization ) {
    if( forceInitialization ) {
      target.setValue( key.getKey(), 1.0F );
    }
    target.setValue( key.getKey(), orig.getFloat( key.getKey() ) );
    target.setDefault( key.getKey(), orig.getDefaultFloat( key.getKey() ) );
  }

  private static void setDoubleValue( final IPreferenceStore store, 
                                      final OverlayKey key, 
                                      final IPreferenceStore target, 
                                      final boolean forceInitialization ) {
    if( forceInitialization ) {
      target.setValue( key.getKey(), 1.0D );
    }
    target.setValue( key.getKey(), store.getDouble( key.getKey() ) );
    target.setDefault( key.getKey(), store.getDefaultDouble( key.getKey() ) );
  }

  private static void setBooleanValue( final IPreferenceStore store, 
                                       final OverlayKey key, 
                                       final IPreferenceStore target, 
                                       final boolean forceInitialization ) {
    if( forceInitialization ) {
      target.setValue( key.getKey(), true );
    }
    target.setValue( key.getKey(), store.getBoolean( key.getKey() ) );
    target.setDefault( key.getKey(), 
                       store.getDefaultBoolean( key.getKey() ) );
  }
}