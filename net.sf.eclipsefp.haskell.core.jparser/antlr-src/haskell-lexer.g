/*
 * File haskell-lexer.g
 * 
 * This file is an ANTLR grammar file that describes a lexer (scanner)
 * for Haskell.
 *
 * ANTLR is needed to translate this grammar to executable code. It is
 * freely available at http://www.antlr.org
 *
 * Author: Thiago Arrais - thiago.arrais@gmail.com
 */
header {
package net.sf.eclipsefp.haskell.core.jparser;
}

class HaskellLexer extends Lexer;

options	{
    k = 9;
}

tokens {
	MODULE = "module" ;
	WHERE = "where" ;
	IMPORT = "import" ;
	QUALIFIED = "qualified" ;
	AS = "as" ;
	HIDING = "hiding" ;
	TYPE = "type" ;
	DATA = "data" ;
	NEWTYPE = "newtype" ;
	CLASS = "class" ;
	INSTANCE = "instance" ;
	DEFAULT = "default" ;
	OFTYPE = "::" ;
	LET = "let" ;
	DO = "do" ;
	OF = "of" ;
	CONTEXT_ARROW = "=>" ;
	LEFT_CURLY = "{" ;
	RIGHT_CURLY = "}" ;
	SEMICOLON = ";" ;
	LEFT_PAREN = "(" ;
	RIGHT_PAREN = ")" ;
	COMMA = "," ;
	DOT = "." ;
	EQUALS = "=" ;

}

{
	/* workaround for starting token coordinates from 0
	 * as eclipse expects them to */
    protected Token makeToken(int t) {
    	Token result = super.makeToken(t);
    	
    	result.setLine(result.getLine() - 1);
    	result.setColumn(result.getColumn() - 1);
    	
    	return result;
    }
}

WS	:
		(' '
    |  	'\t'
    |  	'\r')+
    	{ $setType(Token.SKIP); }
    ;
    
PPDIRECTIVE
	:	'#' (~('\n'))* NEWLINE { $setType(Token.SKIP); }
	;


CONSTRUCTOR_ID : UPPER_CASE	( LETTER
							| DIGIT
							| '\'' )* ;
							
VARIABLE_ID : LOWER_CASE	( LETTER
							| DIGIT
							| '\'' )* ;

DECIMAL : '0' | ('1'..'9') (DIGIT)* ;

CHARACTER_LITERAL : '\''! (~('\''|'\\')|CHARACTER_ESCAPE) '\''! ;

STRING_LITERAL : '"'! (~('"'|'\\')|STRING_ESCAPE|GAP!)* '"'! ;

protected
CHARACTER_ESCAPE
	:	'\\'!
		( CHAR_ESC
		| ASCII
		| t:DECIMAL {	char c = (char) Integer.parseInt(t.getText());
						setText(Character.toString(c));
					}
		)
	;
	
protected CHAR_ESC
	:
    	( 'a'!
    	| 'b'  { $setText("\b"); }
    	| 'f'  { $setText("\f"); }
    	| 'n'  { $setText("\n"); }
    	| 'r'  { $setText("\r"); }
    	| 't'  { $setText("\t"); }
    	| 'v'!
    	| '\\' { $setText("\\"); }
    	| '\"' { $setText("\""); }
    	| '\'' { $setText("'"); } )
	;

protected
STRING_ESCAPE
	:
		"\\&"! | CHARACTER_ESCAPE
    ;
    
protected
ASCII : "NUL" { setText("\u0000"); } ;
    
COMMENT : LINE_COMMENT | BLOCK_COMMENT ;

protected
LINE_COMMENT : "--" (~'\n')* NEWLINE ;

protected
BLOCK_COMMENT : "{-" (options {greedy=false;} : (NEWLINE | .))* "-}" ;

NEWLINE : '\n' { newline(); } ;

protected
GAP : '\\' (WS | NEWLINE)+ '\\' ;

protected
UPPER_CASE : 'A'..'Z';

protected
LOWER_CASE : ('a'..'z'|'_');

protected
LETTER : UPPER_CASE | LOWER_CASE;

protected
DIGIT : '0'..'9';

SYMBOL : ~('a'..'z' | 'A'..'Z' | '0'..'9');