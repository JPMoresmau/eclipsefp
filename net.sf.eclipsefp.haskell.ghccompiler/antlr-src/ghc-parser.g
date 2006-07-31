/* *****************************************************************************
 * Copyright (c) 2005, 2006 Thiago Arrais and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Thiago Arrais - Initial API and implementation
 * *****************************************************************************
 *
 * File haskell-parser.g
 * 
 * This file is an ANTLR grammar file that describes a parser for GHC 6.4.2
 * output. This grammar is very likely to work with other versions of GHC, but
 * untested.
 *
 * ANTLR is needed to translate this grammar to executable code. It is
 * freely available at http://www.antlr.org
 *
 * Author: Thiago Arrais - thiago.arrais@gmail.com
 */
header 
{
//This GhcOutputParser.java file is automatically generated
//DO NOT CHANGE THIS FILE DIRECTLY
//Change the ghc-parser.g file and re-generate it instead

package net.sf.eclipsefp.haskell.ghccompiler.core;

import java.io.Reader;
import java.io.StringReader;

import net.sf.eclipsefp.haskell.core.compiler.CompilerOutput;
import net.sf.eclipsefp.haskell.core.compiler.CompilerOutputItem;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutput;

}

class GhcOutputParser extends Parser;

{
    
    private CompilerOutput fOutput = new CompilerOutput();
    
    /**
     * Convenience method for parsing strings
     */
    public static ICompilerOutput parse(String input) {
        final Reader inputReader = new StringReader(input);
        final GhcOutputParser parser = new GhcOutputParser(
                                           new GhcOutputLexer(inputReader));
        try {
            parser.output();
        } catch(Throwable t) {
            //ignore parsing errors and just return what we have for now
        }

        return parser.getParsedOutput();
    }
    
    public ICompilerOutput getParsedOutput() {
        return fOutput;
    }
}

output : (chasing)? (error)+ ;

chasing : CHASING_FROM COLON TEXT NL
          ((COMPILING|SKIPPING) not_nl NL)+ ;

error
    {
        final CompilerOutputItem item = new CompilerOutputItem();
        
        String message = "";
    }
    :
    	NL fileName:TEXT { item.setFileName(fileName.getText()); }
    	COLON line:TEXT { item.setLine(Integer.parseInt(line.getText())); }
    	COLON range:TEXT {  String[] arrRange = range.getText().split("-");
    	                    item.setStartColumn(Integer.parseInt(arrRange[0]));
    	                    item.setEndColumn(Integer.parseInt(arrRange[1])); }
    	COLON message=not_nl { item.setComment(message.trim()); }
    	NL
    	{
    		fOutput.addError(item);
    	}
    ;

not_nl returns [String result]
    {
        StringBuffer buf = new StringBuffer();
        
        result = "";
    }
    :
        ( t:TEXT { buf.append(t.getText()); }
        | c:COLON { buf.append(c.getText()); }
        )+
    { result = buf.toString(); }
    ;
    
class GhcOutputLexer extends Lexer;

options	{
    k = 10;
}

tokens {
    CHASING_FROM = "Chasing modules from" ;
}

COMPILING : "Compiling" ;
SKIPPING : "Skipping" ;
TEXT : (~(':' | '\r' | '\n' ))+;

COLON : ':' ;
NL : '\r' '\n' | '\n' ;