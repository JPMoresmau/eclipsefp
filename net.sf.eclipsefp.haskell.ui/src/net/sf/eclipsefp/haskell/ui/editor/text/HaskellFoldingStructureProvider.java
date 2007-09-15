// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.editor.text;

import java.util.*;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.projection.ProjectionAnnotation;
import org.eclipse.jface.text.source.projection.ProjectionAnnotationModel;

import net.sf.eclipsefp.haskell.core.halamo.*;
import net.sf.eclipsefp.haskell.ui.editor.HaskellEditor;

/** <p>provides folding regions for documents in the Haskell editor.</p>
  *
  * @author Leif Frenzel
  */
class HaskellFoldingStructureProvider {

  private final HaskellEditor editor;
  private IDocument document;
  private IProgressMonitor progressMonitor;
  
  HaskellFoldingStructureProvider( final HaskellEditor editor ) {
    this.editor = editor;
  }

  void setProgressMonitor( final IProgressMonitor progressMonitor ) {
    this.progressMonitor = progressMonitor;
  }

  void setDocument( final IDocument document ) {
    this.document = document;
  }
  
  void updateFoldingRegions( final ICompilationUnit cu ) { 
    ProjectionAnnotationModel model = getAnnModel();
    if( model != null ) {
      Set currentRegions = new HashSet();
      addFoldingRegions( cu, currentRegions );
      updateFoldingRegions( model, currentRegions );
    }
  }

  private void addFoldingRegions( final ICompilationUnit cu, 
                                  final Set regions ) {
    IModule[] modules = cu.getModules();
    for( int i = 0; i < modules.length; i++ ) {
      IModule module = modules[ i ];
      addImports( cu, module, regions );
      addMultiLineDeclarations( cu, module, regions );
      addFunctionBindings( cu, module, regions );
    }
  }

  private void addFunctionBindings( final ICompilationUnit cu, 
                                    final IModule module, 
                                    final Set regions ) {
    IDeclaration[] decls = module.getDeclarations();
    for( int i = 0; i < decls.length; i++ ) {
      if( decls[ i ] instanceof IFunctionBinding ) {
        IMatch[] matches = ( ( IFunctionBinding )decls[ i ] ).getMatches();
        if( matches.length > 0 ) {
          int startLine = matches[ 0 ].getSourceLocation().getLine();
          IMatch lastMatch = matches[ matches.length - 1 ];
          ISourceLocation lastLoc = lastMatch.getSourceLocation();
          ISourceLocation nextLoc = cu.getNextLocation( lastLoc );
          int endLine = ( nextLoc == null ) ? document.getNumberOfLines() 
                                            : nextLoc.getLine();
          handleSection( startLine, endLine, regions );
        }        
      }
    }
  }

  private void addImports( final ICompilationUnit cu, 
                           final IModule module, 
                           final Set regions ) {
    IImport[] imports = module.getImports();
    if( imports.length > 0 ) {
      int startLine = imports[ 0 ].getSourceLocation().getLine();
      IImport lastImport = imports[ imports.length - 1 ];
      ISourceLocation lastImportLoc = lastImport.getSourceLocation();
      ISourceLocation nextLoc = cu.getNextLocation( lastImportLoc );
      
      int endLine = ( nextLoc == null ) ? document.getNumberOfLines() 
                                        : nextLoc.getLine();
      handleSection( startLine, endLine, regions );
    }
  }

  private void addMultiLineDeclarations( final ICompilationUnit cu, 
                                         final IModule module, 
                                         final Set regions ) {
    ISourceLocation loc = module.getSourceLocation();
    ISourceLocation nextLoc = cu.getNextLocation( loc );
    while( nextLoc != null ) {
      int startLine = loc.getLine();
      int endLine = nextLoc.getLine();
      handleSection( startLine, endLine, regions );
      ISourceLocation tempLoc = nextLoc;
      nextLoc = cu.getNextLocation( loc );
      loc = tempLoc;
    }
  }

  private void handleSection( final int startLine, 
                              final int endLine, 
                              final Set regions ) {
    int newEndLine = endLine;
    if( newEndLine > 0 ) {
      newEndLine--;
    }
    newEndLine = countBackEmptyLines( newEndLine );
    if( startLine < newEndLine ) {
      Position position = createPosition( startLine, newEndLine );
      if( position != null ) {
        regions.add( position );
      }
    }
  }

  private Position createPosition( final int startLine, final int endLine ) {
    Position result = null;
    try {
      int start = document.getLineOffset( startLine );
      int end =   document.getLineOffset( endLine ) 
                + document.getLineLength( endLine );
      result = new Position(start, end - start);
    } catch( final BadLocationException  badlox ) {
      // ignored
      badlox.printStackTrace();
    }
    return result;
  }

  private int countBackEmptyLines( final int endLine ) {
    int result = endLine;
    try {
      int offset = document.getLineOffset( endLine ); 
      int length = document.getLineLength( endLine );
      String line = document.get( offset, length );
      if( line == null || line.trim().length() == 0 ) {
        result = countBackEmptyLines( endLine - 1 );
      }
    } catch( final BadLocationException badlox ) {
      // ignored
      badlox.printStackTrace();
    }
    return result;
  }

  private ProjectionAnnotationModel getAnnModel() {
    Class cls = ProjectionAnnotationModel.class;
    return ( ProjectionAnnotationModel )editor.getAdapter( cls );
  }
  
  private void updateFoldingRegions( final ProjectionAnnotationModel model, 
                                     final Set currentRegions ) {
    Annotation[] deletions = computeDifferences( model, currentRegions );
    
    Map additionsMap = new HashMap();
    Iterator it = currentRegions.iterator();
    while( it.hasNext() ) {
      additionsMap.put( new ProjectionAnnotation(), it.next() );
    }
    
    if(    ( deletions.length != 0 || additionsMap.size() != 0 ) 
        && !isCancelled() ) {
      model.modifyAnnotations( deletions, additionsMap, new Annotation[] {} );
    }
  }

  private boolean isCancelled() {
    return progressMonitor != null && ( progressMonitor.isCanceled() );
  }
  
  private Annotation[] computeDifferences( final ProjectionAnnotationModel mdl, 
                                           final Set current ) {
    List deletions= new ArrayList();
    Iterator iter = mdl.getAnnotationIterator();
    while( iter.hasNext() ) {
      Object annotation = iter.next();
      if( annotation instanceof ProjectionAnnotation ) {
        Position position = mdl.getPosition( ( Annotation )annotation );
        if( current.contains( position ) ) {
          current.remove( position );
        } else {
          deletions.add( annotation );
        }
      }
    }
    Annotation[] result = new Annotation[ deletions.size() ];
    return ( Annotation[] )deletions.toArray( result );
  }
}
