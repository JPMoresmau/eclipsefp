package net.sf.eclipsefp.haskell.core.jparser;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

import net.sf.eclipsefp.haskell.core.jparser.ast.CompilationUnit;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Status;

import antlr.RecognitionException;
import antlr.TokenStreamException;

import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import de.leiffrenzel.fp.haskell.core.parser.IHaskellParser;

public class JavaParserBridge implements IHaskellParser {

	public ICompilationUnit parse(IFile file) throws CoreException {
		Reader input;
		if (isLiterate(file)) {
			input = new LiterateHaskellReader(
						new InputStreamReader(
						    file.getContents()));
		} else {
			input = new InputStreamReader(
				    	file.getContents());
		}
		HaskellParser parser = new HaskellParser(input);
		CompilationUnit result = null;
		try {
			result = new CompilationUnit(parser.parseModule());
			result.setUnderlyingResource(file);
		} catch (RecognitionException e) {
			raiseCoreException(e, "Parsing error on " + file.getName());
		} catch (TokenStreamException e) {
			raiseCoreException(e, "Scanning error on " + file.getName());
		} finally {
			try {
				input.close();
			} catch (IOException e) {
				raiseCoreException(e, "Cannot close " + file.getName());
			}
		}
		return result;
	}

	private void raiseCoreException(Throwable cause, String msg)
		throws CoreException
	{
		throw new CoreException(
				new Status(Status.ERROR, JParserPlugin.getPluginId(),
						   -1, msg, cause ));
	}

	private boolean isLiterate(IFile file) {
		final String fileName = file.getName();
		return fileName != null && fileName.endsWith("lhs");
	}

	public boolean canParse() {
		return true;
	}

}
