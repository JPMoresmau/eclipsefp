package net.sf.eclipsefp.haskell.core.jparser;

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
		try {
			return new CompilationUnit(parser.parseModule());
		} catch (RecognitionException e) {
			throw new CoreException(
					new Status(Status.ERROR,
							   JParserPlugin.getPluginId(),
							   -1,
							   "Parsing error on " + file.getName(),
							   e ));
		} catch (TokenStreamException e) {
			throw new CoreException(
					new Status(Status.ERROR,
							   JParserPlugin.getPluginId(),
							   -1,
							   "Scanning error on " + file.getName(),
							   e ));
		}
	}

	private boolean isLiterate(IFile file) {
		final String fileName = file.getName();
		return fileName != null && fileName.endsWith("lhs");
	}

	public boolean canParse() {
		return true;
	}

}
