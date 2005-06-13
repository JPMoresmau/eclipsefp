// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.core;

import java.util.*;

/** <p>encapsulates a list of filenames and info whether they are active or 
  * not.</p>
  *
  * @author Leif Frenzel
  */
public class InterfaceList {

  private List list = new ArrayList();
  
  public void add( final InterfaceListEntry entry ) {
    if(     entry != null 
        &&  !entry.getFileName().equals( "" ) 
        && !contains( entry )) {
      list.add( entry );
    }
  }

  public void remove( final InterfaceListEntry entry ) {
    list.remove( entry );
  }
  
  public InterfaceListEntry[] getAll() {
    InterfaceListEntry[] result = new InterfaceListEntry[ list.size() ];
    list.toArray( result );
    return result;
  }
  
  public void selectAll() {
    setAll( true );
  }
  
  public void deselectAll() {
    setAll( false );
  }
  
  
  // helping methods
  //////////////////

  private void setAll( final boolean toWhat ) {
    Iterator it = list.iterator();
    while( it.hasNext() ) {
      ( ( InterfaceListEntry )it.next() ).setUsed( toWhat );
    }
  }
  
  private boolean contains( final InterfaceListEntry entry ) {
    boolean result = false;
    Iterator it = list.iterator();
    while( !result && it.hasNext() ) {
      InterfaceListEntry next = ( InterfaceListEntry )it.next();
      result = entry.getFileName().equals( next.getFileName() );
    }
    return result;
  }
}
