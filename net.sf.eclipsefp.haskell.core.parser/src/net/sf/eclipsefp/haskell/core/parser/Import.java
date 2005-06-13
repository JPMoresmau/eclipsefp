// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import java.util.ArrayList;
import java.util.List;

import de.leiffrenzel.fp.haskell.core.halamo.*;

/** <p>implementation for <code>IImport</code>, represents an import 
  * statement.</p>
  *
  * @author Leif Frenzel
  */
class Import implements IImport {

  // an import statement has no name itself (only a reference to an imported
  // element), so we give a general dummy name here
  private static final String IMPORT_NAME = "import";

  private final IModule module;
  private String importedElement;
  private boolean hiding;
  private List importSpecs;
  private ISourceLocation sourceLocation;
  
  Import( final IModule module, final CompilationUnit cu, final int pos ) {
    this.module = module;
    importSpecs = new ArrayList();
    init( cu, pos );
  }

  private void setImportedElement( final String importedElement ) {
    this.importedElement = importedElement;
  }

  private void setSourceLocation( final ISourceLocation sourceLocation ) {
    this.sourceLocation = sourceLocation;
  }
  
  private void setHiding( final boolean hiding ) {
    this.hiding = hiding;
  }
  
  private void addImportSpecification( final IImportSpecification importSpec ) {
    importSpecs.add( importSpec );
  }
  
  
  // interface methods of IImport
  ///////////////////////////////

  public ICompilationUnit getCompilationUnit() {
    return module.getCompilationUnit();
  }
  
  public IModule getModule() {
    return module;
  }

  public String getImportedElement() {
    return importedElement;
  }

  public String getName() {
    return IMPORT_NAME;
  }

  public ISourceLocation getSourceLocation() {
    return sourceLocation;
  }

  public IHaskellLanguageElement getParent() {
    return module;
  }
  
  public IImportSpecification[] getImportSpecifications() {
    int size = importSpecs.size();
    IImportSpecification[] result = new IImportSpecification[ size ];
    importSpecs.toArray( result );
    return result;
  }
  
  public boolean isHiding() {
    return hiding;
  }
  
  
  // helping methods
  //////////////////
  
  private void init( final CompilationUnit cu, final int pos ) {
    String elem = NativeParser.getImportedElement( cu.getHandle(), pos );
    setImportedElement( elem );
    initImportSpecs( cu.getHandle(), pos );
    int line = NativeParser.getImportLine( cu.getHandle(), pos );
    int column = NativeParser.getImportColumn( cu.getHandle(), pos );
    SourceLocation srcLoc = new SourceLocation( line, column );
    setSourceLocation( srcLoc );
    cu.mapSourceLocation( srcLoc, this );
  }
  
  private void initImportSpecs( final int handle, final int impPos ) {
    int count = NativeParser.getImportSpecificationCount( handle, impPos );
    if( count > 0 ) {
      boolean hiding = NativeParser.isImportSpecificationHiding( handle, 
                                                                 impPos );
      setHiding( hiding );
    }
    for( int i = 0; i < count; i++ ) {
      int type = NativeParser.getImportSpecificationType( handle, impPos, i );
      ImportSpecification impSpec = createImpSpec( type );
      String name 
        = NativeParser.getImportSpecificationName( handle, impPos, i );
      impSpec.setName( name );
      addImportSpecification( impSpec );
      
      if( type == IElementTypes.IMPORT_THING_WITH ) {
        int compCount 
          = NativeParser.getImportThingWithCount( handle, impPos, i );
        for( int j = 0; j < compCount; j++ ) {
          String compName 
            = NativeParser.getImportThingWithName( handle, impPos, i, j );
          ImportThingWith impTW = ( ImportThingWith )impSpec;
          impTW.addComponent( new ImportThingWithComponent( impTW, compName ) );
        }
      }
    }
  }

  private ImportSpecification createImpSpec( final int type ) {
    ImportSpecification result = null;
    switch( type ) {
      case IElementTypes.IMPORT_VARIABLE:
        result = new ImportVariable( this );
        break;
      case IElementTypes.IMPORT_ABSOLUTE:
        result = new ImportAbsolute( this );
        break;
      case IElementTypes.IMPORT_THING_ALL:
        result = new ImportThingAll( this );
        break;
      case IElementTypes.IMPORT_THING_WITH:
        result = new ImportThingWith( this );
        break;
      default:
        throw new IllegalArgumentException( "Import spec type " + type );
    }
    return result;
  }
}
