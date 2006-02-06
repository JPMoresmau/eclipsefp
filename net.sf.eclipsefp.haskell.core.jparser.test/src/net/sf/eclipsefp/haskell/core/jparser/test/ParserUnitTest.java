package net.sf.eclipsefp.haskell.core.jparser.test;

import java.io.StringReader;

import de.leiffrenzel.fp.haskell.core.halamo.IDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IExportSpecification;
import de.leiffrenzel.fp.haskell.core.halamo.IImport;
import de.leiffrenzel.fp.haskell.core.halamo.IMatch;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;
import antlr.CommonToken;
import antlr.RecognitionException;
import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;
import net.sf.eclipsefp.haskell.core.jparser.EclipseFPToken;
import net.sf.eclipsefp.haskell.core.jparser.HaskellLexer;
import net.sf.eclipsefp.haskell.core.jparser.HaskellLexerTokenTypes;
import net.sf.eclipsefp.haskell.core.jparser.HaskellParser;
import net.sf.eclipsefp.haskell.core.jparser.ModuleBuilder;
import net.sf.eclipsefp.haskell.core.jparser.QualifiedIdentifierFilter;
import junit.framework.TestCase;

public class ParserUnitTest extends TestCase {
	
	private HaskellParser fParser;
	private MockModuleBuilder fMockBuilder;

	@Override
	protected void setUp() throws Exception {
		fMockBuilder = new MockModuleBuilder();
		fParser = new HaskellParser(new TestTokenStream(),
									fMockBuilder);
	}

	public void testCallingBuilder() throws RecognitionException, TokenStreamException {
		fParser.parseModule();
		
		assertEquals("startModule:;addImport;addImport;", fMockBuilder.getLog());
	}
	
	public void testCorrectParse() throws RecognitionException, TokenStreamException {
		IModule module = fParser.parseModule();
		
		assertEquals("ParserTest", module.getName());
		
		IImport[] imps = module.getImports();
		assertEquals(2, imps.length);
		assertEquals("LibraryL.ModuleM", imps[0].getName());
		assertEquals("LibraryK.ModuleN", imps[1].getName());
	}
	
	public void testEmptyDeclaration() throws RecognitionException, TokenStreamException {
		final String inStr = "module ParserTest where {" +
				             " test = putStr \"Hello, world!\" ;" +
				             " ;" +
				             " main = test" +
				             "}";
		fParser = new HaskellParser(
				    new HaskellLexer(
				     new StringReader(inStr)));
		
		IModule module = fParser.parseModule();
		
		IDeclaration[] decls = module.getDeclarations();
		assertEquals(2, decls.length);
	}
	
	public void testEmptyImport() throws RecognitionException, TokenStreamException {
		final String inStr = "module ParserTest where {" +
		                     " import LibraryL.ModuleM ;" +
		                     " ;" +
		                     " import LibraryL.ModuleN" +
		                     "}";
		fParser = new HaskellParser(
				   new QualifiedIdentifierFilter(
				    new HaskellLexer(
					 new StringReader(inStr))));
		
		IModule module = fParser.parseModule();
		
		IImport[] imps = module.getImports();
		assertEquals(2, imps.length);
	}
	
	public void testEmptyImportsBeforeDeclarations() throws RecognitionException, TokenStreamException {
		final String inStr = "module ParserTest where {" +
		                     " import LibraryL.ModuleM ;" +
		                     " import LibraryL.ModuleN ;" +
		                     " ; ; " +
		                     " fat 0 = 1" +
		                     "}";
		fParser = new HaskellParser(
				   new QualifiedIdentifierFilter(
				    new HaskellLexer(
					 new StringReader(inStr))));
		
		IModule module = fParser.parseModule();
		
		assertEquals(2, module.getImports().length);
		assertEquals(1, module.getDeclarations().length);
	}

	/** a stream that gives the following sequence of tokens
	 * <code>
	 *     module ParserTest where {
	 *         import LibraryL.ModuleM ;
	 *         import LibraryK.ModuleN
	 *     }
	 * </code>
	 */
	private static class TestTokenStream implements TokenStream, HaskellLexerTokenTypes {

		private Token[] fTokens = new Token[] {
				new EclipseFPToken(MODULE, "module"),
				new EclipseFPToken(CONSTRUCTOR_ID, "ParserTest"),
				new EclipseFPToken(WHERE, "where"),
				new EclipseFPToken(LEFT_CURLY, "{"),
				new EclipseFPToken(IMPORT, "import"),
				new EclipseFPToken(QCONID, "LibraryL.ModuleM"),
				new EclipseFPToken(SEMICOLON, ";"),
				new EclipseFPToken(IMPORT, "import"),
				new EclipseFPToken(QCONID, "LibraryK.ModuleN"),
				new EclipseFPToken(RIGHT_CURLY, "}"),
				new EclipseFPToken(EOF, "<<eof>>")
		};
		
		private int currentToken = 0;
		
		public Token nextToken() throws TokenStreamException {
			return fTokens[currentToken++];
		}
	}
	
	private static class MockModuleBuilder extends ModuleBuilder {
		
		private StringBuffer fLog = new StringBuffer();

		@Override
		public void addDeclaration(IDeclaration declaration) {
			fLog.append("addDeclaration;");
			super.addDeclaration(declaration);
		}

		public String getLog() {
			return fLog.toString();
		}

		@Override
		public void addExport(IExportSpecification export) {
			fLog.append("addExport;");
			super.addExport(export);
		}

		@Override
		public void addFunctionMatch(IMatch match) {
			fLog.append("addFunctionMatch;");
			super.addFunctionMatch(match);
		}

		@Override
		public void addImport(IImport theImport) {
			fLog.append("addImport;");
			super.addImport(theImport);
		}

		@Override
		public IModule startModule(String moduleName) {
			fLog.append("startModule:");
			fLog.append(moduleName);
			fLog.append(";");
			return super.startModule(moduleName);
		}
		
	}
}
