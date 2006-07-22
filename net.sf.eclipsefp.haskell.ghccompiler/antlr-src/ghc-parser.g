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
import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutput;

}

class GhcOutputParser extends Parser;

{
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
        return new CompilerOutput();
    }
}

output : LETTER ;

class GhcOutputLexer extends Lexer;

LETTER : ( 'a'..'z'|'A'..'Z') ;