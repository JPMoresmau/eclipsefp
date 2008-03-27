// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.compiler;

import java.io.Writer;
import org.eclipse.core.resources.IFile;

/**
 * Implementations know how to compile a haskell file.
 *
 * @author Leif Frenzel
 * @author Thiago Arrais
 */
public interface IHaskellCompiler {

	/**
	 * Compiles the specified file redirecting any compiler output to the given
	 * streams.
	 */
    void compile(IFile file, Writer outputWriter);

	/**
	  * Compiles the specified file.
	  *
	  * Any output is ignored.
	  */
	 void compile(IFile file);

}