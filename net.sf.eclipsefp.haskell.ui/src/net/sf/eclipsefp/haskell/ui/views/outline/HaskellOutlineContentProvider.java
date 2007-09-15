// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.outline;

import java.util.ArrayList;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.*;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.IDocumentProvider;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.halamo.*;


/** <p>Structures the editor's document into a tree-like model and 
  * provides elements for the elements of the tree, to display them on a
  * tree viewer.</p>
  * 
  * @author Leif Frenzel
  */
public class HaskellOutlineContentProvider implements ITreeContentProvider {

  public final static String POS_CAT_MODULE = "__haskell_module";
  private final IPositionUpdater positionUpdater 
    = new DefaultPositionUpdater( POS_CAT_MODULE );
    
  private ArrayList<IModule> content = new ArrayList<IModule>( 10 );

  private final IDocumentProvider documentProvider;
  private Object input;


  HaskellOutlineContentProvider( final IDocumentProvider documentProvider,
                                 final Object input ) {
    this.documentProvider = documentProvider;
    this.input = input;
  }

  void setInput( final Object input ) {
    this.input = input;
  }

  public void inputChanged( final Viewer viewer, 
                            final Object oldInput, 
                            final Object newInput ) {
    if( oldInput != null ) {
      IDocument document = documentProvider.getDocument( oldInput );
      if( document != null ) {
        try {
          document.removePositionCategory( POS_CAT_MODULE );
        } catch( BadPositionCategoryException bapocax ) {
          // ignore this, we know the position exists
        }
        document.removePositionUpdater( positionUpdater );
      }
    }
    content.clear();

    if( newInput != null ) {
      FileEditorInput fei = ( FileEditorInput )newInput;
      IDocument document = documentProvider.getDocument( newInput );
      if( document != null ) {
        document.addPositionCategory( POS_CAT_MODULE );
        document.addPositionUpdater( positionUpdater );
        parse( fei.getFile() );
      }
    }
  }


  // methods of TreeContentProvider
  /////////////////////////////////
  
  public Object[] getElements( final Object element ) {
    return content.toArray(); 
  }
  
  public boolean hasChildren( final Object element ) {
    boolean result = element == input;
    if( element instanceof IModule ) {
      result = ( ( IModule )element ).getImports().length > 0;
    } 
    return result;
  }

  public Object[] getChildren( final Object element ) {
    Object[] result;
    if( element == input ) {
      result = content.toArray();
    } else if( element instanceof IModule ) {
      result = ( ( IModule )element ).getImports();
    } else {
      result = new Object[ 0 ];
    }
    return result;
  }

  public Object getParent( final Object element ) {
    Object result = null;
    if( element instanceof IModule ) {
      result = input;
    }
    return result;
  }
  
  public void dispose() {
    if( content != null ) {
      content.clear();
      content = null;
    }
  }


  // helping methods
  //////////////////
  
  private void parse( final IFile file ) {
	HaskellModelManager halamo = HaskellCorePlugin.getDefaultModelManager();
	ICompilationUnit cu = halamo.getCompilationUnit( file );
    IModule[] modules = cu.getModules();
    // old TODO remove
    for( int i = 0; i < modules.length; i++ ) {
      content.add( modules[ i ] );
    }
  }
}