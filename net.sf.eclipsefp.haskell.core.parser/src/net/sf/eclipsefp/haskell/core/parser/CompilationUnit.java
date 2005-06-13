// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import java.util.*;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;

import de.leiffrenzel.fp.haskell.core.halamo.*;

/** <p>holds a parsed compilation unit (represents a Haskell source file), 
  * with a handle to the parse result from the native parser.</p>
  * 
  * @author Leif Frenzel
  */
class CompilationUnit implements ICompilationUnit {

  private final int hsParseResultHandle;
  private final IFile underlyingResource;
  private IModule[] modules;

  private List srcLocs;
  private Map srcLocElems;
  
  CompilationUnit( final int hsParseResultHandle, final IFile file ) {
    this.hsParseResultHandle = hsParseResultHandle;
    this.underlyingResource = file;
    srcLocs = new ArrayList();
    srcLocElems = new HashMap();
    // assume there is only one module per compilation unit -
    // this is how the parser handles things
    modules = new IModule[] { new Module( this ) };
  }

  int getHandle() {
    return hsParseResultHandle;
  }
  
  void mapSourceLocation( final ISourceLocation srcLoc, 
                          final IHaskellLanguageElement elem ) {
    srcLocs.add( srcLoc );
    srcLocElems.put( srcLoc, elem );
  }
  
  // interface methods of IAdaptable
  //////////////////////////////////
  
  public Object getAdapter( final Class adapter ) {
    Object result = null;
    if( adapter.getClass() == IResource.class ) {
      result = underlyingResource;
    }
    return result;
  }
  
  
  // interface methods of ICompilationUnit
  ////////////////////////////////////////
  
  public IModule[] getModules() {
    return modules;
  }

  public IFile getUnderlyingResource() {
    return underlyingResource;
  }
  
  public ISourceLocation getNextLocation( final ISourceLocation srcLoc ) {
    ISourceLocation result = null;
    // special case: only one location in the list
    if( srcLocs.size() == 1 ) {
      result = handleOneElementCase( srcLoc );
    } else if( srcLocs.size() > 1 ) {
      // special case: refLoc is before the first location in the list
      ISourceLocation firstLoc = ( ISourceLocation )srcLocs.get( 0 );
      if( srcLoc.isBefore( firstLoc ) ) {
        result = firstLoc;
      } else {
        int index = findLocation( srcLoc );
        index++; // we're interested in the next location
        if( index > 0 && index < srcLocs.size() ) {
          result = ( ISourceLocation )srcLocs.get( index );
        }
      }
    }
    return result;
  }

  private ISourceLocation handleOneElementCase( final ISourceLocation refLoc ) {
    ISourceLocation result = null;
    ISourceLocation srcLoc = ( ISourceLocation )srcLocs.get( 0 );
    if( srcLoc.isAfter( refLoc ) ) {
      result = srcLoc;
    } 
    return result;
  }

  private int findLocation( final ISourceLocation refLoc ) {
    int result = -1;
    if( srcLocs.size() > 1 ) {
      int index = 0;
      while( result == -1 && index < ( srcLocs.size() - 1 ) ) {
        ISourceLocation srcLoc = ( ISourceLocation )srcLocs.get( index );
        ISourceLocation nextLoc = ( ISourceLocation )srcLocs.get( index + 1 );
        if( srcLoc.isBefore( refLoc ) && nextLoc.isAfter( refLoc ) ) {
          result = index;
        } else if( !srcLoc.isAfter( refLoc ) && !srcLoc.isBefore( refLoc ) ) {
          result = index;
        }
        index++;
      }
    }
    return result;
  }
}
