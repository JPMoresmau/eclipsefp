// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import de.leiffrenzel.fp.haskell.core.halamo.*;

/** <p>an implementation for <code>IModule</code> which represents a module
  * in a source file.</p>
  * 
  * @author Leif Frenzel 
  */
class Module implements IModule {
  
  private final CompilationUnit cu;
  private String name;
  private ISourceLocation sourceLocation;
  private IImport[] imports;
  private IExportSpecification[] exports;
  private IDeclaration[] decls;
  
  Module( final CompilationUnit cu ) {
    this.cu = cu;
    init();
  }
  
  
  // interface methods of IModule
  ///////////////////////////////

  public IExportSpecification[] getExportSpecifications() {
    return exports;
  }
  
  public ICompilationUnit getCompilationUnit() {
    return cu;
  }
  
  public IDeclaration[] getDeclarations() {
    return decls;
  }
  
  public IImport[] getImports() {
    return imports;
  }

  public String getName() {
    return name;
  }

  public ISourceLocation getSourceLocation() {
    return sourceLocation;
  }
  
  public IHaskellLanguageElement getParent() {
    // modules have no parent (they are the top-level elements in the language
    // element hierarchy
    return null;
  }

  
  // helping methods
  //////////////////
  
  private void init() {
    name = NativeParser.getModuleName( cu.getHandle() );
    int line = NativeParser.getModuleLine( cu.getHandle() );
    int column = NativeParser.getModuleColumn( cu.getHandle() );
    sourceLocation = new SourceLocation( line, column );
    cu.mapSourceLocation( sourceLocation, this );
    initExports();
    initImports();
    initDecls();
  }

  private void initExports() {
    int handle = cu.getHandle();
    int exportCount = NativeParser.getExportCount( handle );
    this.exports = new IExportSpecification[ exportCount ];
    for( int i = 0; i < exportCount; i++ ) {
      int type = NativeParser.getExportSpecificationType( handle, i );
      ExportSpecification exp = null;
      switch( type ) {
        case IElementTypes.EXPORT_VARIABLE:
          exp = new ExportVariable( this );
          break;
        case IElementTypes.EXPORT_ABSOLUTE:
          exp = new ExportAbsolute( this );
          break;
        case IElementTypes.EXPORT_THING_ALL:
          exp = new ExportThingAll( this );
          break;
        case IElementTypes.EXPORT_THING_WITH:
          exp = new ExportThingWith( this );
          break;
        case IElementTypes.EXPORT_MODULE_CONTENT:
          exp = new ExportModuleContent( this );
          break;
        default:
          throw new IllegalArgumentException( "Export spec type " + type );
      }
      exp.setName( NativeParser.getExportSpecificationName( handle, i ) );
      exports[ i ] = exp;
      
      if( type == IElementTypes.EXPORT_THING_WITH ) {
        int count = NativeParser.getExportThingWithCount( handle, i );
        for( int j = 0; j < count; j++ ) {
          String name = NativeParser.getExportThingWithName( handle, i, j );
          ExportThingWith expTW = ( ExportThingWith )exp;
          expTW.addComponent( new ExportThingWithComponent( expTW, name ) );
        }
      }
    }
  }
  
  private void initImports() {
    int importCount = NativeParser.getImportCount( cu.getHandle() );
    this.imports = new IImport[ importCount ];
    for( int i = 0; i < importCount; i++ ) {
      this.imports[ i ] = new Import( this, cu, i );
    }
  }

  private void initDecls() {
    int declCount = NativeParser.getDeclCount( cu.getHandle() );
    decls = new IDeclaration[ declCount ];
    for( int i = 0; i < declCount; i++ ) {
      int type = NativeParser.getDeclType( cu.getHandle(), i );
      switch( type ) {
        case IElementTypes.TYPE_DECL:
          decls[ i ] = createTypeDecl( i );
          break;
        case IElementTypes.DATA_DECL:
          decls[ i ] = createDataDecl( i );
          break;
        case IElementTypes.INFIX_DECL:
          decls[ i ] = createInfixDecl( i );
          break;
        case IElementTypes.NEWTYPE_DECL:
          decls[ i ] = createNewTypeDecl( i );
          break;
        case IElementTypes.CLASS_DECL:
          decls[ i ] = createClassDecl( i );
          break;
        case IElementTypes.INSTANCE_DECL:
          decls[ i ] = createInstanceDecl( i );
          break;
        case IElementTypes.DEFAULT_DECL:
          decls[ i ] = createDefaultDecl( i );
          break;
        case IElementTypes.TYPE_SIGNATURE:
          decls[ i ] = createTypeSignature( i );
          break;
        case IElementTypes.FUNCTION_BINDING:
          decls[ i ] = new FunctionBinding( this, this, cu, i );
          break;
        case IElementTypes.PATTERN_BINDING:
          decls[ i ] = createPatternBinding( i );
          break;
        default:
          throw new IllegalArgumentException( "Declaration type " + type );
      }
    }
  }

  private IDeclaration createInfixDecl( final int pos ) {
    Declaration result = new InfixDeclaration( this, this, cu, pos );
    readSourceLocation( result, pos );
    return result;
  }
  
  private IDeclaration createPatternBinding( final int pos ) {
    Declaration result = new PatternBinding( this, this );
    readSourceLocation( result, pos );
    return result;
  }
  
  private IDeclaration createDataDecl( final int pos ) {
    Declaration result = new DataDeclaration( this, this, cu, pos );
    readCommonInfo( result, pos );
    return result;
  }
  
  private IDeclaration createDefaultDecl( final int pos ) {
    Declaration result = new DefaultDeclaration( this, this );
    readSourceLocation( result, pos );
    return result;
  }

  private IDeclaration createClassDecl( final int pos ) {
    Declaration result = new ClassDeclaration( this, this, cu, pos );
    readCommonInfo( result, pos );
    return result;
  }

  private Declaration createTypeDecl( final int pos ) {
    Declaration result = new TypeDeclaration( this, this );
    readCommonInfo( result, pos );
    return result;
  }
  
  private Declaration createNewTypeDecl( final int pos ) {
    Declaration result = new NewTypeDeclaration( this, this );
    readCommonInfo( result, pos );
    return result;
  }
  private Declaration createInstanceDecl( final int pos ) {
    Declaration result = new InstanceDeclaration( this, this );
    readCommonInfo( result, pos );
    return result;
  }

  private void readCommonInfo( final Declaration decl, final int pos ) {
    decl.setName( NativeParser.getDeclName( cu.getHandle(), pos ) );
    readSourceLocation( decl, pos );
  }

  private void readSourceLocation( final Declaration decl, final int pos ) {
    int line = NativeParser.getDeclLine( cu.getHandle(), pos );
    int column = NativeParser.getDeclColumn( cu.getHandle(), pos );
    SourceLocation srcLoc = new SourceLocation( line, column );
    decl.setSourceLocation( srcLoc );
    cu.mapSourceLocation( srcLoc, decl );
  }
  
  private ITypeSignature createTypeSignature( final int pos ) {
    int handle = cu.getHandle();

    TypeSignature sig = new TypeSignature( this, this );
    int line = NativeParser.getDeclLine( handle, pos );
    int column = NativeParser.getDeclColumn( handle, pos );
    SourceLocation srcLoc = new SourceLocation( line, column );
    sig.setSourceLocation( srcLoc );
    cu.mapSourceLocation( srcLoc, sig );

    int count = NativeParser.getTypeSigIdentifierCount( handle, pos );
    for( int i = 0; i < count; i++ ) {
      sig.addIdentifier( NativeParser.getTypeSigIdentifier( handle, pos, i ) );
    }
    return sig;
  }
}