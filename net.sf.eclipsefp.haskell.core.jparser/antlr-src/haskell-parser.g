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
//This HaskellParser.java file is automatically generated
//DO NOT CHANGE THIS FILE DIRECTLY
//Change the haskell.parser.g file and re-generate it instead

package net.sf.eclipsefp.haskell.core.jparser;
	
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.List;
import java.util.Vector;

import de.leiffrenzel.fp.haskell.core.halamo.IDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IExportSpecification;
import de.leiffrenzel.fp.haskell.core.halamo.IImport;
import de.leiffrenzel.fp.haskell.core.halamo.IImportSpecification;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;

import net.sf.eclipsefp.haskell.core.jparser.ast.ClassDeclaration;
import net.sf.eclipsefp.haskell.core.jparser.ast.DataDeclaration;
import net.sf.eclipsefp.haskell.core.jparser.ast.Declaration;
import net.sf.eclipsefp.haskell.core.jparser.ast.DefaultDeclaration;
import net.sf.eclipsefp.haskell.core.jparser.ast.ExportSpecification;
import net.sf.eclipsefp.haskell.core.jparser.ast.FunctionBinding;
import net.sf.eclipsefp.haskell.core.jparser.ast.HaskellLanguageElement;
import net.sf.eclipsefp.haskell.core.jparser.ast.Import;
import net.sf.eclipsefp.haskell.core.jparser.ast.InstanceDeclaration;
import net.sf.eclipsefp.haskell.core.jparser.ast.Module;
import net.sf.eclipsefp.haskell.core.jparser.ast.NewtypeDeclaration;
import net.sf.eclipsefp.haskell.core.jparser.ast.TypeSynonymDeclaration;

}

class HaskellParser extends Parser;

options {
	importVocab = HaskellLexer;
}

//extra code for HaskellParser class
{
    public HaskellParser(InputStream in) {
        this(new InputStreamReader(in));
    }
    
    public HaskellParser(Reader in) {
    	this(new HaskellFormatter(new HaskellCommentFilter(new HaskellLexer(in))));	
    }
    
    private <T extends HaskellLanguageElement> T createNode(Class<T> nodeClazz)
    	throws TokenStreamException
    {
		T result;
		try {
    		result = nodeClazz.newInstance();
		} catch (Exception e) {
			throw new RuntimeException("Could not instantiate class " + nodeClazz.getName(),
			                           e);
		}
		
		Token nextToken = LT(1);
		result.setLocation(nextToken.getLine(), nextToken.getColumn());
		
		return result;
    }
}

parseModule returns [IModule result]
	{ result = null; }
	: result=module
	;

module returns [IModule result]
    {
        Module aModule = createNode(Module.class);
        
        String name = null;
        IModule aBody = null;
        List<IExportSpecification> someExports = null;
        result = null;
    }
    :
      ( t:MODULE
        name=modid { aModule.setName(name); }
        ( someExports=exports { aModule.addExports(someExports); } )?
        WHERE aBody=body
    | aBody=body )
    {
    	aModule.addImports(aBody.getImports());
    	aModule.addDeclarations(aBody.getDeclarations());
        result = aModule;
    }
    ;

qconid returns [String result]
	{
		StringBuffer buf = new StringBuffer();
		result = null;
	}
	:
		id:CONSTRUCTOR_ID { buf.append(id.getText()); }
		(
			DOT t:CONSTRUCTOR_ID
			{
				buf.append('.');
				buf.append(t.getText());
			}
		)*
		{
			result = buf.toString();
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
    	ExportSpecification anExport = createNode(ExportSpecification.class);
    	String name = null;
    	result = null;
    }
    :
    (
    	name = qvar
    |
    	name=qtyconorcls ( LEFT_PAREN
    	                   ((~(RIGHT_PAREN))*)
    	                   RIGHT_PAREN
    	                 )?
   	|
   		MODULE name=modid
    )
    {
   		anExport.setName(name);
   	    result = anExport;
    }
    ;
    
qtyconorcls returns [String result]
	{
		result = null;
	}
	:
		result=qconid
	;
    
cnamelist
	:
		cname (COMMA cname)*
	;
	
cname
	:
		VARIABLE_ID | CONSTRUCTOR_ID
	;
    
qvar returns [String result] 
	{
		StringBuffer buf = new StringBuffer();
		result = null;
	}
	:
		id:VARIABLE_ID { buf.append(id.getText()); }
		(
			DOT t:VARIABLE_ID
			{
				buf.append('.');
				buf.append(t.getText());
			}
		)*
		{
			result = buf.toString();
		}
	;
	
varid returns [String result]
	{
		result = null;
	}
	:
		id:VARIABLE_ID { result = id.getText(); }
	;
	
modid returns [String result]
	{
		result = null;
	}
	: 
	    result = qconid
	    { return result; }
	;

conid returns [String result]
	{
		result = null;
	}
	:
		t:CONSTRUCTOR_ID { result = t.getText(); }
	;
          
body returns [Module result]
	{
	    result = createNode(Module.class);

	    List<IImport> imports;
	    List<IDeclaration> decls;
	}
	:
		(
		LEFT_CURLY
			(
				imports=impdecls { result.addImports(imports); }
				(
					SEMICOLON
					decls=topdecls { result.addDeclarations(decls); }
				)?
			|
				decls=topdecls { result.addDeclarations(decls); }
			)
		RIGHT_CURLY
		)
	;
	
impdecls returns [List<IImport> result]
	{
		IImport anImport;
		result = new Vector<IImport>();
	}
	:
		anImport=impdecl { result.add(anImport); }
		( (SEMICOLON IMPORT) =>
		SEMICOLON anImport=impdecl { result.add(anImport); } )*
	;

impdecl returns [IImport result]
	{
		Import anImport = createNode(Import.class);
		
		String name = null;
		List<IImportSpecification> someSpecs = null;
		result = null;
	}
	:
		(
			t:IMPORT
			(QUALIFIED)?
			name=modid
			(AS modid)?
			(someSpecs=impspec { anImport.addSpecifications(someSpecs); } )?
		)
		{
			anImport.setElementName(name);
			result = anImport;
		}
	;

impspec returns [List<IImportSpecification> result]
	{
		result = new Vector<IImportSpecification>(0);
	}
	:
	    (HIDING)?
	    list
	;

topdecls returns [List<IDeclaration> result]
	{
		result = new Vector<IDeclaration>();
		
		IDeclaration aDeclaration = null;
	}
	:
		(		
			aDeclaration=topdecl { result.add(aDeclaration); }
			( SEMICOLON aDeclaration=topdecl { result.add(aDeclaration); })*
		)?
	;

topdecl returns [IDeclaration result]
	{
		result = null;

	}
	:
		result=typesymdecl
	|
		result=data_or_rnmdtypedecl
	|
		result=classdecl
	|
		result=instancedecl
	|
		result=defaultdecl
	|
		result=decl
	;
	
typesymdecl returns [IDeclaration result]
	{
		Declaration aDeclaration = createNode(TypeSynonymDeclaration.class);
		result = aDeclaration;
		
		String name = null;
	}
	:
		TYPE
		name=simpletype { aDeclaration.setName(name); }
		declrhs
	;
	
data_or_rnmdtypedecl returns [IDeclaration result]
	{
		Declaration aDeclaration = createNode(Declaration.class);
		
		result = null;
		String name = null;
	}
	:
		(
			DATA { aDeclaration = new DataDeclaration(); }
		|
			NEWTYPE { aDeclaration = new NewtypeDeclaration(); }
		)
		((context CONTEXT_ARROW) => context CONTEXT_ARROW)?
		name=simpletype { aDeclaration.setName(name); }
		declrhs
		{
			result = aDeclaration;
		}
	;
	
classdecl returns [IDeclaration result]
	{
		ClassDeclaration aDeclaration = createNode(ClassDeclaration.class);
		String name = null;
		result = aDeclaration;
	}
	:
		CLASS
		((context CONTEXT_ARROW) => context CONTEXT_ARROW)?
		name=conid { aDeclaration.setName(name); }
		varid
		(
			WHERE
			block
		)?
	;
	
instancedecl returns [IDeclaration result]
	{
		InstanceDeclaration aDeclaration = createNode(InstanceDeclaration.class);
		result = aDeclaration;
		
		String name = null;
	}
	:
		INSTANCE
		((context CONTEXT_ARROW) => context CONTEXT_ARROW)?
		qconid
		name=qconid { aDeclaration.setName(name); }
		(
			WHERE
			block
		)?
	;

defaultdecl returns [IDeclaration result]
	{
		result = createNode(DefaultDeclaration.class);
	}
	:
		DEFAULT list
	;

context
	:
		(~(CONTEXT_ARROW))*
	;
	
simpletype returns [String result]
	{
		result = null;
	}
	:
		id:CONSTRUCTOR_ID { result = id.getText(); }
		(~(EQUALS))*
	;
	
decl returns [IDeclaration result]
	{
		FunctionBinding decl = createNode(FunctionBinding.class);
		result = decl;

		Token name = null;
	}
	:
		name=funlhs {	decl.setName(name.getText()); }
		declrhs
	;

funlhs returns [Token result]
	{
		result = null;
	}
	:
		id:VARIABLE_ID { result=id; } (~(EQUALS))*
	;
	
declrhs :
		EQUALS (block | ~(SEMICOLON | RIGHT_CURLY))*
	;

block : LEFT_CURLY (~( LEFT_CURLY | RIGHT_CURLY ) | block )* RIGHT_CURLY
     ;
     
list : LEFT_PAREN (~( LEFT_PAREN | RIGHT_PAREN ) | list)* RIGHT_PAREN ;

