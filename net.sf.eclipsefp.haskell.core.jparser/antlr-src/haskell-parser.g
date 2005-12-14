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
import net.sf.eclipsefp.haskell.core.jparser.ast.FunctionMatch;
import net.sf.eclipsefp.haskell.core.jparser.ast.HaskellLanguageElement;
import net.sf.eclipsefp.haskell.core.jparser.ast.Import;
import net.sf.eclipsefp.haskell.core.jparser.ast.InstanceDeclaration;
import net.sf.eclipsefp.haskell.core.jparser.ast.Module;
import net.sf.eclipsefp.haskell.core.jparser.ast.NewtypeDeclaration;
import net.sf.eclipsefp.haskell.core.jparser.ast.TypeSignature;
import net.sf.eclipsefp.haskell.core.jparser.ast.TypeSynonymDeclaration;

}

class HaskellParser extends Parser;

options {
	importVocab = HaskellLexer;
}

//extra code for HaskellParser class
{
	private ModuleBuilder fBuilder;

    public HaskellParser(InputStream in) {
        this(new InputStreamReader(in));
    }
    
    public HaskellParser(Reader in) {
    	this(new HaskellFormatter(new HaskellCommentFilter(new HaskellLexer(in))),
    		 new ModuleBuilder());	
    }
    
	public HaskellParser(TokenStream stream, ModuleBuilder builder) {
		this(stream);
		fBuilder = builder;
	}
    
    
    public IModule parseModule() throws RecognitionException, TokenStreamException {
		module();
		return fBuilder.getResult();
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
    
    private static class NonNullVector<E> extends Vector<E> {
		@Override
		public synchronized boolean add(E elem) {
			if (elem == null)
				return false;
			else
				return super.add(elem);
		}
    }
    
}

module
    {
        Module aModule = (Module) fBuilder.startModule();
        
		Token nextToken = LT(1);
        aModule.setLocation(nextToken.getLine(), nextToken.getColumn());
        
        String name = null;
    }
    :
      	(	t:MODULE
        	name=modid { aModule.setName(name); }
        	(exports)?
        	WHERE
      	)?
      	body
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

exports
	:
	    LEFT_PAREN
	    (exportlist)? (COMMA)?
	    RIGHT_PAREN
	;
	
exportlist
	:
		export 
		(COMMA  export)*
	;

export
    {
    	ExportSpecification anExport = createNode(ExportSpecification.class);
    	fBuilder.addExport(anExport);
    	
    	String name = null;
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
	}
	:
		(
		LEFT_CURLY
			(
				impdecls
				( SEMICOLON topdecls )?
			|
				topdecls
			)
		RIGHT_CURLY
		)
	;
	
impdecls
	:
		impdecl
		( (SEMICOLON IMPORT) =>
		  SEMICOLON impdecl )*
	;

impdecl
	{
		Import anImport = createNode(Import.class);
		fBuilder.addImport(anImport);
		
		String name = null;
		List<IImportSpecification> someSpecs = null;
	}
	:
		(
			t:IMPORT
			(QUALIFIED)?
			name=modid { anImport.setElementName(name); }
			(AS modid)?
			(someSpecs=impspec { anImport.addSpecifications(someSpecs); } )?
		)
	;

impspec returns [List<IImportSpecification> result]
	{
		result = new Vector<IImportSpecification>(0);
	}
	:
	    (HIDING)?
	    list
	;

topdecls
	:
		(		
			topdecl
			( SEMICOLON topdecl )*
		)?
	;

topdecl 
	:	typesymdecl
	|   data_or_rnmdtypedecl
	|   classdecl
	|   instancedecl
	|   defaultdecl
	|   decl
	;
	
typesymdecl
	{
		Declaration aDeclaration = createNode(TypeSynonymDeclaration.class);
		fBuilder.addDeclaration(aDeclaration);
		
		String name = null;
	}
	:
		TYPE
		name=simpletype { aDeclaration.setName(name); }
		declrhs
	;
	
data_or_rnmdtypedecl
	{
		Declaration aDeclaration = createNode(Declaration.class);

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
		    fBuilder.addDeclaration(aDeclaration);
		}
	;
	
classdecl
	{
		ClassDeclaration aDeclaration = createNode(ClassDeclaration.class);
		fBuilder.addDeclaration(aDeclaration);

		String name = null;
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
	
instancedecl
	{
		InstanceDeclaration aDeclaration = createNode(InstanceDeclaration.class);
		fBuilder.addDeclaration(aDeclaration);
		
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

defaultdecl
	{
		IDeclaration aDeclaration = createNode(DefaultDeclaration.class);
		fBuilder.addDeclaration(aDeclaration);
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
	
decl
	:
		(vars OFTYPE) => signdecl	
	|	(funlhs EQUALS) => valdef
	|   nonstddecl
	;
	
//matches almost anything
//used to ignore non-standard declarations
nonstddecl
	:
		(block | ~(SEMICOLON|RIGHT_CURLY))+
	;

valdef
	{
		FunctionMatch match = createNode(FunctionMatch.class);
		
		Token name = null;
	}
	:
		name=funlhs {	match.setName(name.getText());
						fBuilder.addFunctionMatch(match);
					}
		declrhs
	;

signdecl
	{
		TypeSignature tsig = createNode(TypeSignature.class);
		fBuilder.addDeclaration(tsig);		
		
		String name = null;
	}
	:
		name=vars { tsig.setName(name); }
		OFTYPE
		(~(SEMICOLON | RIGHT_CURLY))*
	;
	
vars returns [String result]
	{
		StringBuffer buf = new StringBuffer();
		result = null;
		
		String var = null;
	}
	:
		var=varid { buf.append(var); }
		(
			COMMA
			var=varid {
						buf.append(", ");
		          		buf.append(var);
		          	  }
		)*
		{
			result = buf.toString();
		}
	;

funlhs returns [Token result]
	{
		result = null;
	}
	:
		id:VARIABLE_ID { result=id; } (~(EQUALS|SEMICOLON))*
	;
	
declrhs :
		EQUALS (block | ~(SEMICOLON | RIGHT_CURLY))*
	;

block : LEFT_CURLY (~( LEFT_CURLY | RIGHT_CURLY ) | block )* RIGHT_CURLY
     ;
     
list : LEFT_PAREN (~( LEFT_PAREN | RIGHT_PAREN ) | list)* RIGHT_PAREN ;

