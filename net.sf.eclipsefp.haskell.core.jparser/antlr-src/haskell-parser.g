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

import de.leiffrenzel.fp.haskell.core.halamo.IModule;

import net.sf.eclipsefp.haskell.core.jparser.ast.ClassDeclaration;
import net.sf.eclipsefp.haskell.core.jparser.ast.DataDeclaration;
import net.sf.eclipsefp.haskell.core.jparser.ast.Declaration;
import net.sf.eclipsefp.haskell.core.jparser.ast.DefaultDeclaration;
import net.sf.eclipsefp.haskell.core.jparser.ast.ExportAbsolute;
import net.sf.eclipsefp.haskell.core.jparser.ast.ExportModuleContent;
import net.sf.eclipsefp.haskell.core.jparser.ast.ExportSpecification;
import net.sf.eclipsefp.haskell.core.jparser.ast.ExportThingAll;
import net.sf.eclipsefp.haskell.core.jparser.ast.ExportThingWith;
import net.sf.eclipsefp.haskell.core.jparser.ast.ExportThingWithComponent;
import net.sf.eclipsefp.haskell.core.jparser.ast.ExportVariable;
import net.sf.eclipsefp.haskell.core.jparser.ast.FunctionMatch;
import net.sf.eclipsefp.haskell.core.jparser.ast.HaskellLanguageElement;
import net.sf.eclipsefp.haskell.core.jparser.ast.Import;
import net.sf.eclipsefp.haskell.core.jparser.ast.ImportSpecification;
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
	private ModuleBuilder fBuilder = new ModuleBuilder();

    public HaskellParser(InputStream in) {
        this(new InputStreamReader(in));
    }
    
    public HaskellParser(Reader in) {
    	this(new HaskellFormatter(new HaskellLexer(in)),
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

		recordNextTokenLocation(result);		
		
		return result;
    }
    
    private void recordNextTokenLocation(HaskellLanguageElement node)
    	throws TokenStreamException
    {
		Token nextToken = LT(1);
		node.setLocation(nextToken.getLine(), nextToken.getColumn());
    }
    
    private <T extends Declaration> T insertNewDeclaration(Class<T> nodeClazz)
    	throws TokenStreamException
	{    
    	T decl = createNode(nodeClazz);
    	fBuilder.addDeclaration(decl);
    	return decl;
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
      	(	MODULE
        	name=modid { aModule.setName(name); }
        	(exports)?
        	WHERE
      	)?
      	body
    ;

qconid returns [String result]
	{
		result = null;
	}
	:
		t1:QCONID { result = t1.getText(); }
	|	t2:CONSTRUCTOR_ID { result = t2.getText(); }
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
		((COMMA  (qvar | qtyconorcls | MODULE)) => COMMA  export)*
	;

export
    {
    	ExportSpecification anExport = null;
    	ExportThingWith anExportWith = null;
    	ExportThingWithComponent aComponent = null;
    	
		String compName = null;
    	String name = null;
    }
    :
    (
    	name = qvar { anExport = createNode(ExportVariable.class); }
    |
    	name=qtyconorcls { anExport = createNode(ExportAbsolute.class); }
    	( LEFT_PAREN
    	  ( t:VARSYM { "..".equals(t.getText()) }? { anExport = createNode(ExportThingAll.class); }
    	  | { anExport = anExportWith = createNode(ExportThingWith.class); }
    	    ( { aComponent = createNode(ExportThingWithComponent.class); }
    	      compName=cname {	aComponent.setName(compName);
    	      					anExportWith.addComponent(aComponent); }
    	      ( COMMA { aComponent = createNode(ExportThingWithComponent.class); }
    	        compName=cname {	aComponent.setName(compName);
    	      						anExportWith.addComponent(aComponent); } )*
    	    | qvar (COMMA qvar)* ) )?
    	  RIGHT_PAREN )?
   	|
   		MODULE name=modid { anExport = createNode(ExportModuleContent.class); }
    )
    {
		fBuilder.addExport(anExport);
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
	
cname returns [String result]
	{
		result = null;
	}
	:
		t1:VARIABLE_ID { result = t1.getText(); }
	|	t2:CONSTRUCTOR_ID { result = t2.getText(); }
	;
    
qvar returns [String result] 
	{
		result = null;
	}
	:
		result=qvarid
	|	LEFT_PAREN result=qvarsym RIGHT_PAREN
	;
	
qvarid returns [String result]
	{
		result = null;
	}
	:
		t1:QVARID { result = t1.getText(); }
	|	t2:VARIABLE_ID { result = t2.getText(); }
	;

qvarsym returns [String result]
	{
		result = null;
	}
	:
		t1:QVARSYM { result = t1.getText(); }
	|	t2:VARSYM { result = t2.getText(); }
	;
	
modid returns [String result]
	{
		result = null;
	}
	: 
	    result = qconid
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
		( (SEMICOLON (IMPORT | SEMICOLON) ) =>
		  SEMICOLON impdecl )*
	;

impdecl
	{
		Import anImport = createNode(Import.class);
		
		String name = null;
		List<ImportSpecification> someSpecs = null;
	}
	:
		(
			IMPORT { fBuilder.addImport(anImport); }
			(QUALIFIED)?
			name=modid { anImport.setElementName(name); }
			(AS modid)?
			( (HIDING { anImport.setHiding(true); } )?
			  someSpecs=impspec { anImport.addSpecifications(someSpecs); } )?
		)
	| //empty declaration
	;

impspec returns [List<ImportSpecification> result]
	{
		ImportSpecification anImpSpec = null;

		result = new Vector<ImportSpecification>();
	}
	:
	    LEFT_PAREN
	    ( anImpSpec=imp { result.add(anImpSpec); }
	      ( (COMMA imp)
	      => COMMA anImpSpec=imp { result.add(anImpSpec); } )*
	      (COMMA)?)?
	    RIGHT_PAREN
	;
	
imp returns [ImportSpecification result]
	{
		ImportSpecification anImport = createNode(ImportSpecification.class);
		result = anImport;
		
		String specName = null;
	}
	:
		specName=var { anImport.setName(specName); }
	|	specName=tyconorcls { anImport.setName(specName); }
	   	( LEFT_PAREN
    	  ( t:VARSYM { "..".equals(t.getText()) }?
    	  | cname (COMMA cname)*
    	  | var (COMMA var)* )?
    	  RIGHT_PAREN )?
	;
	
tyconorcls returns [String result]
	{
		result = null;
	}
	:
		result = conid
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
	|   datadecl
	|	rnmdtypedecl
	|   classdecl
	|   instancedecl
	|   defaultdecl
	|   decl
	;
	
typesymdecl
	{
		Declaration aDeclaration = insertNewDeclaration(TypeSynonymDeclaration.class);
		
		String name = null;
	}
	:
		TYPE
		name=simpletype { aDeclaration.setName(name); }
		declrhs
	;
	
datadecl
	{
		Declaration aDeclaration = insertNewDeclaration(DataDeclaration.class);

		String name = null;
	}
	:
		DATA
		((context CONTEXT_ARROW) => context CONTEXT_ARROW)?
		name=simpletype { aDeclaration.setName(name); }
		declrhs
	;
	
rnmdtypedecl
	{
		Declaration aDeclaration = insertNewDeclaration(NewtypeDeclaration.class);

		String name = null;
	}
	:
		NEWTYPE
		((context CONTEXT_ARROW) => context CONTEXT_ARROW)?
		name=simpletype { aDeclaration.setName(name); }
		declrhs
	;
	
classdecl
	{
		ClassDeclaration aDeclaration = fBuilder.startClassDeclaration();
		recordNextTokenLocation(aDeclaration);

		String name = null;
	}
	:
		CLASS
		((context CONTEXT_ARROW) => context CONTEXT_ARROW)?
		name=conid { aDeclaration.setName(name); }
		tyvar
		(
			WHERE
			cdecls
		)?
		{ fBuilder.endClassDeclaration(); }
	;
	
instancedecl
	{
		InstanceDeclaration aDeclaration = insertNewDeclaration(InstanceDeclaration.class);
		
		String name = null;
	}
	:
		INSTANCE
		((context CONTEXT_ARROW) => context CONTEXT_ARROW)?
		qconid
		name=inst { aDeclaration.setName(name); }
		(
			WHERE
			block
		)?
	;

inst returns [String result]
	{
		result=null;
	}
	:	result=gtycon
	|   LEFT_PAREN result=gtycon (tyvar)* RIGHT_PAREN
	|	LEFT_BRACKET result=conid RIGHT_BRACKET {result = '[' + result + ']';}
	;
		
gtycon returns [String result]
	{
		result = null;
	}
	: result=qtyconorcls
	;

defaultdecl
	{
		insertNewDeclaration(DefaultDeclaration.class);
	}
	:
		DEFAULT list
	;

cdecls
	:
		LEFT_CURLY
		( cdecl (SEMICOLON cdecl)* )?
		RIGHT_CURLY	
	;
	
cdecl
	:
		(vars OFTYPE) => signdecl
	|	nonstddecl
	;

context
	:
		(~(CONTEXT_ARROW|SEMICOLON))*
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
	|   //empty declaration
	;
	
//matches almost anything
//used to ignore non-standard declarations
nonstddecl
	:
		(block | ~(SEMICOLON|RIGHT_CURLY))+
	;

//the valdef rule doesn't come directly from the report spec
//it is inherited from the Language.Haskell.Parser impl
valdef
	{
		FunctionMatch match = createNode(FunctionMatch.class);
		
		String name = null;
	}
	:
		name=funlhs {	match.setName(name);
						fBuilder.addFunctionMatch(match);
					}
		declrhs
	;

signdecl
	{
		TypeSignature tsig = createNode(TypeSignature.class);
		fBuilder.addTypeSignature(tsig);
		
		String[] names = null;
	}
	:
		names=vars { tsig.setIdentifiers(names); }
		OFTYPE
		(~(SEMICOLON | RIGHT_CURLY))*
	;
	
vars returns [String[] result]
	{
		List<String> buf = new Vector<String>(10);
		result = null;
		
		String var = null;
	}
	:
		var=var { buf.add(var); }
		(
			COMMA
			var=var	{ buf.add(var); }
		)*
		{
			result = buf.toArray(new String[buf.size()]);
		}
	;
	
var returns [String result]
	{
		result = null;
	}
	:
		id:VARIABLE_ID { result = id.getText(); }
	|	LEFT_PAREN varsymID:VARSYM { result = varsymID.getText(); } RIGHT_PAREN
	;

tyvar : VARIABLE_ID ;

funlhs returns [String result]
	{
		String infixID;
		String prefixedID;
		result = null;
	}
	:
		(	(VARIABLE_ID varop) =>
		        VARIABLE_ID
		        infixID=varop { result = infixID; }
		|	prefixedID=var { result = prefixedID; } )
		(block | ~(EQUALS|SEMICOLON))*
	;
	
varop returns [String result]
	{
		result = null;
	}
	:	
		t1:VARSYM { result=t1.getText(); }
	|	INFIX_QUOTE
		t2:VARIABLE_ID { result = t2.getText(); }
		INFIX_QUOTE
	;
	
declrhs :
		EQUALS (block | ~(SEMICOLON | RIGHT_CURLY))*
	;

block : LEFT_CURLY (~( LEFT_CURLY | RIGHT_CURLY ) | block )* RIGHT_CURLY
     ;
     
list : LEFT_PAREN (~( LEFT_PAREN | RIGHT_PAREN ) | list)* RIGHT_PAREN ;

