package net.sf.eclipsefp.haskell.core.uuagc;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Scanner;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;


public class UuagcProjectManager {

  public static final String UUAGC_OPTIONS_FILENAME = "uuagc_options"; //$NON-NLS-1$
  private static final String FILE_INIT = "file"; //$NON-NLS-1$
  private static final String OPTIONS_INIT = "options"; //$NON-NLS-1$

  private final IProject project;
  private ArrayList<UuagcFile> files;

  public UuagcProjectManager( final IProject project ) {
    this.project = project;
  }

  public void initFromProject() {
    IPath path = new Path( UUAGC_OPTIONS_FILENAME );
    if( project.exists( path ) ) {
      try {
        InputStream stream = project.getFile( path ).getContents();
        String contents = new Scanner( stream ).useDelimiter( "\\Z" ).next(); //$NON-NLS-1$
        initFromContents( contents );
      } catch( CoreException e ) {
        initFromContents( "" ); //$NON-NLS-1$
      }
    } else {
      initFromContents( "" ); //$NON-NLS-1$
    }
  }

  public void initFromContents( final String contents ) {
    // Initialize the files
    files = new ArrayList<UuagcFile>();
    // Parse the file
    String[] lines = contents.split( "[\r\n]+" ); //$NON-NLS-1$
    for (int i = 0; i < lines.length; i++) {
      String line = lines[i];
      if (line.startsWith( FILE_INIT )) {
        // Get filename
        int colonPos = line.indexOf( ':' );
        String filenameWithQuotes = line.substring( colonPos + 1 ).trim();
        String filename = filenameWithQuotes.substring( 1, filenameWithQuotes.length() - 1 );

        // Try to find options
        for (i++; i < lines.length; i++) {
          line = lines[i];
          if (line.startsWith( OPTIONS_INIT )) {
            colonPos = line.indexOf( ':' );
            String options = line.substring( colonPos + 1 ).trim();
            ArrayList<String> optionsList = new ArrayList<String>(  );
            for (String option : options.split( "[ ]*,[ ]*" )) { //$NON-NLS-1$
              optionsList.add( option );
            }
            files.add( new UuagcFile( filename, optionsList ) );
          }
        }
      }
    }
  }

  public String toUuagcString() {
    StringBuilder builder = new StringBuilder();
    for (UuagcFile file : files) {
      if (builder.length() > 0) {
        builder.append( "\n\n" ); //$NON-NLS-1$
      }
      builder.append( file.toUuagcString() );
    }
    return builder.toString();
  }

  public void save() throws CoreException {
    TextFileDocumentProvider provider = new TextFileDocumentProvider();
    IFile optionsFile = project.getFile( UUAGC_OPTIONS_FILENAME );
    provider.connect( optionsFile );
    IDocument doc = provider.getDocument( optionsFile );
    doc.set( toUuagcString() );
    provider.saveDocument( null, optionsFile, doc, true );
  }

  public List<UuagcFile> getElements() {
    return Collections.unmodifiableList( files );
  }

  public void addElement( final UuagcFile file ) {
    files.add( file );
  }
}
