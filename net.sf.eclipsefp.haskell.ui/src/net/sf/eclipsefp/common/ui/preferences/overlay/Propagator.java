// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.preferences.overlay;

import org.eclipse.jface.preference.IPreferenceStore;


/** <p>helper for the overlay store.</p>
  * 
  * @author Leif Frenzel
  */
class Propagator {

  static void propagate( final IPreferenceStore origin, 
                         final OverlayKey key, 
                         final IPreferenceStore target ) {
    if( OverlayType.BOOLEAN == key.getType() ) {
      propagateBoolean( origin, key, target );
    } else if( OverlayType.DOUBLE == key.getType() ) {
      propagateDouble( origin, key, target );
    } else if( OverlayType.FLOAT == key.getType() ) {
      propagateFloat( origin, key, target );
    } else if( OverlayType.INT == key.getType() ) {
      propagateInt( origin, key, target );
    } else if( OverlayType.LONG == key.getType() ) {
      propagateLong( origin, key, target );
    } else if( OverlayType.STRING == key.getType() ) {
      propagateString( origin, key, target );
    }
  }

  
  // helping methods
  //////////////////
  
  private static void propagateString( final IPreferenceStore origin, 
                                       final OverlayKey key, 
                                       final IPreferenceStore target ) {
    String originValue = origin.getString( key.getKey() );
    String targetValue = target.getString( key.getKey() );
    if( isEqual( originValue, targetValue ) ) {
      target.setValue( key.getKey(), originValue );
    }
  }

  private static void propagateBoolean( final IPreferenceStore origin, 
                                        final OverlayKey key, 
                                        final IPreferenceStore target ) {
    boolean originValue = origin.getBoolean( key.getKey() );
    boolean targetValue = target.getBoolean( key.getKey() );
    if( targetValue != originValue ) {
      target.setValue( key.getKey(), originValue );
    }
  }

  private static void propagateDouble( final IPreferenceStore origin, 
                                       final OverlayKey key, 
                                       final IPreferenceStore target ) {
    double originValue = origin.getDouble( key.getKey() );
    double targetValue = target.getDouble( key.getKey() );
    if( targetValue != originValue ) {
      target.setValue( key.getKey(), originValue );
    }
  }

  private static void propagateFloat( final IPreferenceStore origin, 
                                      final OverlayKey key, 
                                      final IPreferenceStore target ) {
    float originValue = origin.getFloat( key.getKey() );
    float targetValue = target.getFloat( key.getKey() );
    if( targetValue != originValue ) {
      target.setValue( key.getKey(), originValue );
    }
  }

  private static void propagateInt( final IPreferenceStore origin, 
                                    final OverlayKey key, 
                                    final IPreferenceStore target ) {
    int originValue = origin.getInt( key.getKey() );
    int targetValue = target.getInt( key.getKey() );
    if( targetValue != originValue ) {
      target.setValue( key.getKey(), originValue );
    }
  }

  private static void propagateLong( final IPreferenceStore origin, 
                                     final OverlayKey key, 
                                     final IPreferenceStore target ) {
    long originValue = origin.getLong( key.getKey() );
    long targetValue = target.getLong( key.getKey() );
    if( targetValue != originValue ) {
      target.setValue( key.getKey(), originValue );
    }
  }

  private static boolean isEqual( final String originValue, 
                                  final String targetValue ) {
    return    ( targetValue != null ) 
           && ( originValue != null )
           && !targetValue.equals( originValue );
  }
}