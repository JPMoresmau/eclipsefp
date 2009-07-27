/*******************************************************************************
 * Copyright (c) 2005, 2006 Thiago Arrais and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Thiago Arrais - Initial API and implementation
 *******************************************************************************/
package net.sf.eclipsefp.haskell.ghccompiler.test.core;

import static org.easymock.EasyMock.anyObject;
import static org.easymock.EasyMock.createStrictMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.getCurrentArguments;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import java.io.File;
import java.io.IOException;
import java.io.Writer;
import net.sf.eclipsefp.haskell.core.compiler.IHaskellCompiler;
import net.sf.eclipsefp.haskell.core.internal.project.HaskellProject_PDETestCase;
import net.sf.eclipsefp.haskell.core.util.IProcessRunner;
import net.sf.eclipsefp.haskell.ghccompiler.core.GhcCompiler;
import net.sf.eclipsefp.haskell.ghccompiler.core.IGhcOutputProcessor;
import net.sf.eclipsefp.haskell.scion.types.Location;
import net.sf.eclipsefp.haskell.scion.types.Note;
import org.easymock.IAnswer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

public class GhcCompilerTest_PDETest extends HaskellProject_PDETestCase {

  class ExecuteAnswer implements IAnswer<Process> {
    private final String output;
    public ExecuteAnswer(final String output) {
      this.output = output;
    }
    public Process answer() {
		      Writer out = (Writer)(getCurrentArguments()[1]);
		      try {
            out.write( output );
            out.close();
          } catch( IOException e ) {
            // ignore
          }
		      return null;
		    }
  }

	public void testParseOneErrorResult() throws CoreException, IOException {
    String fileName = "Main.hs";
    String program = "main = putStrLn $ show $ fat 4";
    String output = "[1 of 1] Compiling Main             ( Main.hs, Main.o )\n\nMain.hs:1:25: Not in scope: `fat'\n";
    Note note = new Note(Note.Kind.ERROR, new Location( fileName, 0, 25, 0, 25 ), "Not in scope: `fat'", "");

    IProcessRunner procRunner = createStrictMock(IProcessRunner.class);
		expect(procRunner.executeNonblocking((File) anyObject(), (Writer) anyObject(), (Writer) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject()))
		  .andStubAnswer( new ExecuteAnswer(output) );
		replay(procRunner);

		IGhcOutputProcessor outputProcessor = createStrictMock(IGhcOutputProcessor.class);
		outputProcessor.setWorkingDir( (File)anyObject() );
		outputProcessor.compiling( fileName, 1, 1 );
		outputProcessor.message( note );
		replay(outputProcessor);

    IFile f = createSourceFile(program, fileName);
		IHaskellCompiler compiler = new GhcCompiler(procRunner, outputProcessor);
		compiler.compile(f);
		verify(procRunner);
	}
}
