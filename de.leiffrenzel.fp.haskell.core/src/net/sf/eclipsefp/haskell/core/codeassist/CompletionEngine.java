package net.sf.eclipsefp.haskell.core.codeassist;

import java.io.*;
import java.util.*;

import org.eclipse.core.runtime.CoreException;

import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import de.leiffrenzel.fp.haskell.core.halamo.IDeclaration;

public class CompletionEngine {

	public String[] complete(ICompilationUnit unit, int offset) {
		try {
			String completedToken = scanPreffix(unit, offset);
			List<String> possibilities = computePossibilities(unit);
			List<String> result = filter(possibilities, completedToken);
			return result.toArray(new String[result.size()]);
		} catch (CoreException ex) {
			// TODO Auto-generated catch block
			ex.printStackTrace();
		} catch (IOException ex) {
			// TODO Auto-generated catch block
			ex.printStackTrace();
		}
		return null;
	}
	
	private List<String> filter(List<String> strings, String prefix) {
		List<String> result = new ArrayList<String>(strings.size());
		for (String string : strings) {
			if (string.startsWith(prefix)) {
				result.add(string);
			}
		}
		return result;
	}

	private List<String> computePossibilities(ICompilationUnit unit) {
		List<String> result = new ArrayList<String>();
		result.addAll(Arrays.asList(HaskellSyntax.getClasses()));
		result.addAll(Arrays.asList(HaskellSyntax.getKeywords()));
		for (IDeclaration decl : unit.getModules()[0].getDeclarations()) {
			result.add(decl.getName());
		}
		
		return result;
	}

	private String scanPreffix(ICompilationUnit unit, int offset)
			throws CoreException, IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(unit
				.getUnderlyingResource().getContents()));
		StringBuffer buf = new StringBuffer();
		for (long i = 0; i < offset; ++i) {
			buf.append((char) in.read());
		}
		StringBuffer tokenBuf = new StringBuffer();
		char c = buf.charAt(--offset);
		while (!isWhitespace(c)) {
			tokenBuf.append(c);
			c = buf.charAt(--offset);
		}
		String completedToken = tokenBuf.reverse().toString();
		return completedToken;
	}

	private boolean isWhitespace(char c) {
		return "\r\t\n ".indexOf(c) > -1;
	}

}
