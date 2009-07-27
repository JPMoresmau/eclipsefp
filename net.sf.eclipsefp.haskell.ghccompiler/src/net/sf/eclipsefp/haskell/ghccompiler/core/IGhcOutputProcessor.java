package net.sf.eclipsefp.haskell.ghccompiler.core;

import java.io.File;

/**
 * A listener that processes the GHC output in some way.
 * Instances must be reusable for multiple compilation sessions.
 *
 * @author Thomas ten Cate
 */
public interface IGhcOutputProcessor extends IGhcOutputListener {

  public abstract void setWorkingDir( File file );

}