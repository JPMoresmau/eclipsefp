// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.builder;

import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;


/** <p>The superclass for visitors used in building Haskell projects</p>
  *
  * @author Leif Frenzel
  */
abstract class Visitor {

  private final IProgressMonitor monitor;

  private boolean needBuild=false;
  private boolean needSynchronize=false;


  Visitor( final IProgressMonitor monitor ) {
    this.monitor = monitor;
  }

  IProgressMonitor getMonitor() {
    return monitor;
  }


  public boolean isNeedBuild() {
    return needBuild;
  }


  public void setNeedBuild( final boolean needBuild ) {
    this.needBuild = needBuild;
  }

  boolean isHaskellFile( final IFile file ) {
    return FileUtil.hasHaskellExtension( file );
  }

  boolean isCabalFile( final IFile file ) {
    return FileUtil.hasCabalExtension( file );
  }

  boolean isInSourceFolder( final IFile file ) {
    if( file == null || !file.isAccessible() ) {
      throw new IllegalArgumentException();
    }
    return ResourceUtil.isInSourceFolder( file );
  }


  public boolean isNeedSynchronize() {
    return needSynchronize;
  }


  public void setNeedSynchronize( final boolean needSynchronize ) {
    this.needSynchronize = needSynchronize;
  }


}