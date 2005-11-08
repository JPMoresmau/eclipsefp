/*
 * File haskell-parser.g
 * 
 * This file is an ANTLR grammar file that describes a partial parser
 * for Haskell.
 *
 * ANTLR is needed to translate this grammar to executable code. It is
 * freely available at http://www.antlr.org
 *
 * Author: Thiago Arrais - thiago.arrais@gmail.com
 */
header 
{
	package net.sf.eclipsefp.haskell.core.jparser;
}

class HaskellParser extends Parser;

options {
	importVocab = HaskellLexer;
}

parseModule : MODULE CONSTRUCTOR_ID WHERE body
            | body
            ;
            
body : LEFT_CURLY (~( LEFT_CURLY | RIGHT_CURLY ) | body )* RIGHT_CURLY
     ;

