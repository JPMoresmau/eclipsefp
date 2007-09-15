// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.mbview;

import org.eclipse.core.resources.IFile;

import net.sf.eclipsefp.haskell.core.project.IImportLibrary;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;


/** <p>contains filter criteria for the ModuleBrowser as value objects.</p>
  * 
  * @author Leif Frenzel
  */
abstract class FilterCriterion {

  static final FilterCriterion SOURCE_FILE          = new SourceFile();
  static final FilterCriterion LITERATE_SOURCE_FILE = new LiterateSourceFile();
  static final FilterCriterion IMPORT_LIBRARY       = new ImportLibrary();
  static final FilterCriterion PROJECT_EXECUTABLE   = new Executable();
  
  static final FilterCriterion[] ALL_CRITERIA = new FilterCriterion[] {
    SOURCE_FILE,
    LITERATE_SOURCE_FILE,
    IMPORT_LIBRARY,
    PROJECT_EXECUTABLE
  };
  
  private FilterCriterion() {
    // prevent instantiation from outside
  }
  
  /** returns the filter criterion that has the specified id, or null,
    * if id matches nothing. */
  static FilterCriterion create( final String id ) {
    FilterCriterion result = null;
    for( int i = 0; result == null && i < ALL_CRITERIA.length; i++ ) {
      if( id.equals( ALL_CRITERIA[ i ].getId() ) ) {
        result = ALL_CRITERIA[ i ];
      }
    }
    return result;
  }
  
  /** returns whether the specified Object mathches this FilterCriterion,
    * i.e. is filtered out. */
  abstract boolean matches( Object element );
  /** returns an identifier for this FilterCriterion which can be used
    * to store a textual reference to it (in mementos) and to restore it
    * with <code>create( String id )</code>.*/
  abstract String getId();
  
  // inner classes
  ////////////////
  
  private static class SourceFile extends FilterCriterion {
    @Override
    boolean matches( final Object element ) {
      boolean result = true;
      if( element instanceof IFile ) {
        IFile file = ( IFile )element;
        if( file.getFileExtension().equals( ResourceUtil.EXTENSION_HS ) ) {
          result = false;
        }
      }
      return result;
    }

    @Override
    public String getId() {
      return "SOURCE_FILE";
    }
    
    @Override
    public String toString() {
      return "Haskell source files (extension .hs)";
    }
  }
  
  private static class LiterateSourceFile extends FilterCriterion {
    @Override
    boolean matches( final Object element ) {
      boolean result = true;
      if( element instanceof IFile ) {
        IFile file = ( IFile )element;
        if( file.getFileExtension().equals( ResourceUtil.EXTENSION_LHS ) ) {
          result = false;
        }
      }
      return result;
    }

    @Override
    String getId() {
      return "LITERATE_SOURCE_FILE";
    }
    
    @Override
    public String toString() {
      return "Literate Haskell source files (extension .lhs)";
    }
  }

  private static class ImportLibrary extends FilterCriterion {
    @Override
    boolean matches( final Object element ) {
      return !( element instanceof IImportLibrary );
    }

    @Override
    String getId() {
      return "IMPORT_LIBRARY";
    }
    
    @Override
    public String toString() {
      return "Import libraries";
    }
  }
  
  private static class Executable extends FilterCriterion {
    @Override
    boolean matches( final Object element ) {
      boolean result = true;
      if(    element instanceof IFile 
          && ResourceUtil.isProjectExecutable( ( IFile ) element ) ) {
        result = false;
      }
      return result;
    }

    @Override
    String getId() {
      return "PROJECT_EXECUTABLE";
    }
    
    @Override
    public String toString() {
      return "Project executables";
    }
  }
}