package net.sf.eclipsefp.haskell.ghccompiler.core;

import net.sf.eclipsefp.haskell.scion.types.Note;

/**
 * Listener that listens for particular messages spit out by the GHC compiler,
 * and processes them in some way.
 *
 * @author Thomas ten Cate
 */
public interface IGhcOutputListener {

  /**
   * Called when GHC starts compiling a file.
   *
   * @param fileName
   *          name of the file that is being compiled, relative to GHC's working
   *          directory
   * @param number
   *          number of the file being compiled
   * @param total
   *          total number of files being compiled
   */
  public void compiling( String fileName, int number, int total );

  /**
   * Called when GHC prints an error or warning message.
   *
   * @param note
   *          the message, containing a location path relative to GHC's working
   *          directory
   */
  public void message( Note note );

}
