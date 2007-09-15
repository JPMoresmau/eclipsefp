// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.outline;

import java.util.*;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.IFileEditorInput;

import net.sf.eclipsefp.haskell.core.halamo.*;
import net.sf.eclipsefp.haskell.core.parser.IHaskellParser;
import net.sf.eclipsefp.haskell.core.parser.ParserManager;
import net.sf.eclipsefp.haskell.ui.views.common.ExportGroup;
import net.sf.eclipsefp.haskell.ui.views.common.ImportGroup;

/** <p>temporary content provider for the outline.</p>
  * 
  * @author Leif Frenzel
  */
public class ExperimentalCP implements ITreeContentProvider {

  private Object input;
  // maps from modules to their import groups
  private final Map<IModule, ImportGroup> importGroups;
  // maps from modules to their export groups  
  private final Map<IModule, ExportGroup> exportGroups;

  
  public ExperimentalCP() {
    importGroups = new HashMap<IModule, ImportGroup>();
    exportGroups = new HashMap<IModule, ExportGroup>();
  }
  
  
  // interface methods of ITreeContentProvider
  ////////////////////////////////////////////
  
  public Object[] getElements( final Object inputElement ) {
    Object[] result = new Object[ 0 ];
    if( input != null && input instanceof IFileEditorInput ) {
      IFileEditorInput fei = ( IFileEditorInput )input;
      ParserManager manager = ParserManager.getInstance();
      IHaskellParser parser = manager.getParser();
      if( parser != null && parser.canParse() ) {
        try {
          ICompilationUnit cu = parser.parse( fei.getFile() );
          result = cu.getModules();
        } catch( final CoreException ex ) {
          result = new Object[] { ex.getMessage() };
          // TODO Auto-generated catch block
          ex.printStackTrace();
        }
      }
    }
    return result;
  }

  public Object[] getChildren( final Object parentElement ) {
    Object[] result = new Object[ 0 ];
    if( parentElement instanceof IModule ) {
      List<Object> children = new ArrayList<Object>();
      IModule module = ( IModule )parentElement;
      if( module.getExportSpecifications().length > 0 ) {
        children.add( getExportGroup( module ) );
      }
      if( module.getImports().length > 0 ) {
        children.add( getImportGroup( module ) );
      }
      children.addAll( Arrays.asList( module.getDeclarations() ) );
      result = children.toArray();
    } else if ( parentElement instanceof ImportGroup ) {
      result = ( ( ImportGroup )parentElement ).getModule().getImports();
    } else if ( parentElement instanceof ExportGroup ) {
      IModule module = ( ( ExportGroup )parentElement ).getModule();
      result = module.getExportSpecifications();
    } else if ( parentElement instanceof IExportThingWith ) {
      result = ( ( IExportThingWith )parentElement ).getComponents();
    } else if ( parentElement instanceof IDeclaration ) {
      result = getDeclarationChildren( parentElement );
    }
    return result;
  }

  public Object getParent( final Object element ) {
    Object result = null;
    if( element instanceof IImport ) {
      IImport imp = ( IImport )element;
      result = getImportGroup( imp.getModule() );
    } else if( element instanceof IExportSpecification ) {
      IExportSpecification exs = ( IExportSpecification )element;
      result = getExportGroup( exs.getModule() );
    } else if( element instanceof ImportGroup ) {
      result = ( ( ImportGroup )element ).getModule();
    } else if( element instanceof ExportGroup ) {
      result = ( ( ExportGroup )element ).getModule();
    } else if( element instanceof IModule ) {
      result = input;
    } else if( element instanceof IHaskellLanguageElement ) {
      result = ( ( IHaskellLanguageElement )element ).getParent(); 
    }
    return result;
  }

  public boolean hasChildren( final Object element ) {
    return getChildren( element ).length > 0;
  }
  
  public void dispose() {
    // unused
  }
  
  public void inputChanged( final Viewer viewer, 
                            final Object oldInput, 
                            final Object newInput ) {
    this.input = newInput;
    importGroups.clear();    
    exportGroups.clear();
  }

  
  // helping methods
  //////////////////
  
  private Object[] getDeclarationChildren( final Object parentElement ) {
    Object[] result = new Object[ 0 ];
    if( parentElement instanceof IDataDeclaration ) {
      IDataDeclaration decl = ( IDataDeclaration )parentElement;
      result = decl.getConstructors();
    } else if( parentElement instanceof IFunctionBinding ) {
      IFunctionBinding funBind = ( IFunctionBinding )parentElement;
      // policy for function bindings: if only one match is involved,
      // display it directly; in that case, we return no children
      if( funBind.getMatches().length > 1 ) {
        result = funBind.getMatches();
      }
    } else if( parentElement instanceof IInfixDeclaration ) {
      IInfixDeclaration decl = ( IInfixDeclaration )parentElement;
      // policy for infix declarations: if only one operator is involved,
      // display it directly; in that case, we return no children
      if( decl.getOperators().length > 1 ) {
        result = decl.getOperators();
      }
    } else if( parentElement instanceof IClassDeclaration ) {
      IClassDeclaration decl = ( IClassDeclaration )parentElement;
      result = decl.getTypeSignatures();
    } else if( parentElement instanceof ITypeSignature ) {
      ITypeSignature signature = ( ITypeSignature )parentElement;
      // policy for type signatures: if only one identifier is bound in the
      // type signature, display that identifier directly; in that case, we 
      // return no children
      if( signature.getIdentifiers().length > 1 ) {
        result = signature.getIdentifiers();
      }
    } else if( parentElement instanceof IFunctionBinding ) {
      IFunctionBinding funBind = ( IFunctionBinding )parentElement;
      result = funBind.getMatches();
    }
    return result;
  }

  private ImportGroup getImportGroup( final IModule module ) {
    ImportGroup result = importGroups.get( module );
    if( result == null ) {
      result = new ImportGroup( module );
      importGroups.put( module, result );
    }
    return result;
  }

  private ExportGroup getExportGroup( final IModule module ) {
    ExportGroup result = exportGroups.get( module );
    if( result == null ) {
      result = new ExportGroup( module );
      exportGroups.put( module, result );
    }
    return result;
  }
}
