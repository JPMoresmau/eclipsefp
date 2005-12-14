package net.sf.eclipsefp.haskell.core.jparser.test;

import de.leiffrenzel.fp.haskell.core.halamo.IDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IExportSpecification;
import de.leiffrenzel.fp.haskell.core.halamo.IImport;
import de.leiffrenzel.fp.haskell.core.halamo.IMatch;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;
import antlr.RecognitionException;
import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;
import net.sf.eclipsefp.haskell.core.jparser.HaskellLexerTokenTypes;
import net.sf.eclipsefp.haskell.core.jparser.HaskellParser;
import net.sf.eclipsefp.haskell.core.jparser.ModuleBuilder;
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
				new MyToken(MODULE, "module"),
				new MyToken(CONSTRUCTOR_ID, "ParserTest"),
				new MyToken(WHERE, "where"),
				new MyToken(LEFT_CURLY, "{"),
				new MyToken(IMPORT, "import"),
				new MyToken(CONSTRUCTOR_ID, "LibraryL"),
				new MyToken(DOT, "."),
				new MyToken(CONSTRUCTOR_ID, "ModuleM"),
				new MyToken(SEMICOLON, ";"),
				new MyToken(IMPORT, "import"),
				new MyToken(CONSTRUCTOR_ID, "LibraryK"),
				new MyToken(DOT, "."),
				new MyToken(CONSTRUCTOR_ID, "ModuleN"),
				new MyToken(RIGHT_CURLY, "}"),
				new MyToken(EOF)
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
	
	private static class MyToken extends Token {
		
		private String fText;
		
		public MyToken(int type, String text) {
			super(type, text);
		}

		public MyToken(int type) {
			super(type);
		}

		@Override
		public String getText() {
			return fText;
		}

		@Override
		public void setText(String t) {
			fText = t;
		}
		
	}
	
}
