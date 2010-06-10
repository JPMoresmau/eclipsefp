package net.sf.eclipsefp.haskell.core.jparser;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;

import net.sf.eclipsefp.haskell.core.jparser.ast.CompilationUnit;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;

import antlr.RecognitionException;
import antlr.TokenStreamException;

import net.sf.eclipsefp.haskell.core.halamo.ICompilationUnit;
import net.sf.eclipsefp.haskell.core.parser.IHaskellParser;

public class JavaParserBridge implements IHaskellParser {

	public ICompilationUnit parse(final IFile file) throws CoreException {
	  if (!file.getWorkspace().isTreeLocked()){
	      file.refreshLocal( 0, new NullProgressMonitor() );
	  }
		String sourceCode = "";
		Reader originalInput = new InputStreamReader(file.getContents()); 
		try {
			sourceCode = readFully(originalInput);
		} catch (IOException e) {
				raiseCoreException(e, "I/O error when reading " + file.getName());
		} finally {
			try {
				originalInput.close();
			} catch (IOException e) {
				raiseCoreException(e, "Cannot close " + file.getName());
			}
		}
		
		return parse(file, sourceCode);
	}

	public ICompilationUnit parse(final IFile file, final String sourceCode) throws CoreException {
		Reader input;
		if (isLiterate(file)) {
			input = new LiterateHaskellReader(
						new StringReader(sourceCode));
		} else {
			input = new StringReader(sourceCode);
		}

		CompilationUnit result = parse(input, sourceCode, file.getName());
		result.setUnderlyingResource(file);
		return result;
	}

	private CompilationUnit parse(final Reader input, final String sourceCode, final String fileName) throws CoreException {
		HaskellParser parser = new HaskellParser(input);
		CompilationUnit result = null;
		try {
			result = new CompilationUnit(parser.parseModule());
			result.setOriginalSourceCode(sourceCode);
		} catch (RecognitionException e) {
			raiseCoreException(e, "Parsing error on " + fileName);
		} catch (TokenStreamException e) {
			raiseCoreException(e, "Scanning error on " + fileName);
		} finally {
			try {
				input.close();
			} catch (IOException e) {
				raiseCoreException(e, "Cannot close " + fileName);
			}
		}
		return result;
	}

	private String readFully(final Reader input) throws IOException {
		StringBuffer buffer = new StringBuffer(1024);
		final int CBUF_SIZE = 256;
		char[] cbuf = new char[CBUF_SIZE];
		int n = 0;
		while(CBUF_SIZE == (n = input.read(cbuf))) {
			buffer.append(cbuf);
		}
		
		if (n > 0) {
			buffer.append(cbuf, 0, n);
		}
		return buffer.toString();
	}

	private void raiseCoreException(final Throwable cause, final String msg)
		throws CoreException
	{
		throw new CoreException(
				new Status(Status.ERROR, JParserPlugin.getPluginId(),
						   -1, msg, cause ));
	}

	private boolean isLiterate(final IFile file) {
		final String fileName = file.getName();
		return fileName != null && fileName.endsWith("lhs");
	}

	public boolean canParse() {
		return true;
	}

}
