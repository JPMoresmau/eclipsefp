/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.search;

import java.util.Collection;
import net.sf.eclipsefp.haskell.buildwrapper.types.SearchResultLocation;
import org.eclipse.core.resources.IFile;


/**
 * @author JP Moresmau
 *
 */
public class SectionSearchResult {
  private final IFile file;
  private final String section;
  private final Collection<SearchResultLocation> locations;

  public SectionSearchResult( final IFile file,final String section,
      final Collection<SearchResultLocation> locations ) {
    super();
    this.file=file;
    this.section = section;
    this.locations = locations;
  }


  /**
   * @return the file
   */
  public IFile getFile() {
    return file;
  }

  /**
   * @return the section
   */
  public String getSection() {
    return section;
  }

  /**
   * @return the locations
   */
  public Collection<SearchResultLocation> getLocations() {
    return locations;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
   return getSection();
  }


  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ( ( file == null ) ? 0 : file.hashCode() );
    result = prime * result + ( ( section == null ) ? 0 : section.hashCode() );
    return result;
  }


  @Override
  public boolean equals( final Object obj ) {
    if( this == obj ) {
      return true;
    }
    if( obj == null ) {
      return false;
    }
    if( getClass() != obj.getClass() ) {
      return false;
    }
    SectionSearchResult other = ( SectionSearchResult )obj;
    if( file == null ) {
      if( other.file != null ) {
        return false;
      }
    } else if( !file.equals( other.file ) ) {
      return false;
    }
    if( section == null ) {
      if( other.section != null ) {
        return false;
      }
    } else if( !section.equals( other.section ) ) {
      return false;
    }
    return true;
  }
}
