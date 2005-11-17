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
	
import java.io.InputStream;
import java.io.Reader;
import java.util.List;
import java.util.Vector;

import de.leiffrenzel.fp.haskell.core.halamo.IExportSpecification;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;

import net.sf.eclipsefp.haskell.core.jparser.ast.ExportSpecification;
import net.sf.eclipsefp.haskell.core.jparser.ast.Module;
}

class HaskellParser extends Parser;

options {
	importVocab = HaskellLexer;
}

//extra code for HaskellParser class
{
    public HaskellParser(InputStream in) {
        this(new HaskellFormatter(new HaskellLexer(in)));
    }
    
}

parseModule returns [IModule result]
	{ result = null; }
	: result=module
	;

module returns [IModule result]
    {
        Module aModule = new Module();
        List<IExportSpecification> someExports = null;
        result = null;
    }
    :
      ( MODULE
        name:CONSTRUCTOR_ID { aModule.setName(name.getText()); }
        ( someExports=exports { aModule.addExports(someExports); } )?
        WHERE body
    | body )
    {
        result = aModule;
    }
    ;
    

exports returns [List<IExportSpecification> result]
    {
    	result = new Vector<IExportSpecification>(0);
    }
	:
	    LEFT_PAREN
	    (result=exportlist)? (COMMA)?
	    RIGHT_PAREN
	;
	
exportlist returns [List<IExportSpecification> result]
	{
		IExportSpecification anExport = null;
    	result = new Vector<IExportSpecification>();
	}
	:
		anExport=export { result.add(anExport); }
		(COMMA  anExport=export { result.add(anExport); } )*
	;

export returns [IExportSpecification result]
    {
    	ExportSpecification anExport = new ExportSpecification();
    	result = null;
    }
    :
    	( id:VARIABLE_ID )
    	{
    	    anExport.setName(id.getText());
    	    result = anExport;
    	}
    ;
          
body : LEFT_CURLY (~( LEFT_CURLY | RIGHT_CURLY ) | body )* RIGHT_CURLY
     ;

