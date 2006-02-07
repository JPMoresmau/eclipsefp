package net.sf.eclipsefp.haskell.core.codeassist;

import java.io.*;
import java.util.*;

import org.eclipse.core.runtime.CoreException;

import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import de.leiffrenzel.fp.haskell.core.halamo.IDeclaration;

public class CompletionEngine {

	public String[] complete(ICompilationUnit unit, int offset) {
		String completedToken;
		try {
			completedToken = getQualifier(unit, offset);
			List<String> possibilities = computePossibilities(unit);
			List<String> result = filter(possibilities, completedToken);
			return result.toArray(new String[result.size()]);
		} catch (Exception ex) {
			//ignore the error and just return an empty result 
		}
		return new String[0];
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

	private String getQualifier( final ICompilationUnit unit, 
			                     final int offset ) throws CoreException, IOException
	{
		StringBuffer contents = readSourceTillOffset(unit, offset);

		int index = offset;
		StringBuffer sb = new StringBuffer();
		String result = "";
		
		boolean finished = false;
		while( !finished && index > 0 ) {
			char ch = contents.charAt(--index);
			if( Character.isLetterOrDigit( ch ) ) {
				sb.append( ch );
			} else if( ch == '\"' || ch == '\'' ) {
//				striong or char literals are not taken into account
				finished = true;
			} else {
//				no ore identifier part, so we use what we have collected
				result = sb.reverse().toString();
				finished = true;
			}
		}
		if( index == 0 ) {
//			the special case where we have collected sth. but have reached the
//			end of the document meanwhile
			result = sb.reverse().toString();
		}
		return result;
	}

	private StringBuffer readSourceTillOffset(final ICompilationUnit unit, final int offset) throws CoreException, IOException {
		InputStream in = unit.getUnderlyingResource().getContents();
		StringBuffer contents = new StringBuffer(offset);
		for(int i = 0; i < offset; ++i) {
			contents.append((char) in.read());
		}
		return contents;
	}

}
