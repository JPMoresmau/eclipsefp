// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.contenttypes;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.content.IContentDescriber;
import org.eclipse.core.runtime.content.IContentDescription;

/** <p>detects programmatically literate Haskell code in LaTex or Bird
  * style.</p>
  *
  * @author Leif Frenzel
  */
public class LiterateContentDescriber implements IContentDescriber {

  // the params used in the plugin.xml to configure this describer
  public static final QualifiedName STYLE = qualify( "style" ); //$NON-NLS-1$
  public static final QualifiedName BIRD = qualify( "bird" ); //$NON-NLS-1$
  public static final QualifiedName LATEX = qualify( "latex" ); //$NON-NLS-1$

  private static final String[] LATEX_SEQS = new String[] {
    "\\begin{code}", "\\end{code}"   //$NON-NLS-1$//$NON-NLS-2$
  };


  // interface methods of IContentDescriber
  /////////////////////////////////////////

  @Override
  public int describe( final InputStream contents,
                       final IContentDescription description ) throws IOException {
    int result = INDETERMINATE;
    if( description != null && description.isRequested( STYLE ) ) {
      result = VALID;
      try (BufferedReader br = getReader( contents, description )) {
        String line = br.readLine();
        boolean latexDetected = false;
        while( line != null && !latexDetected ) {
          latexDetected = containsLatex( line );
          line = br.readLine();
        }
        QualifiedName value = latexDetected ? LATEX : BIRD;
        description.setProperty( STYLE, value );
      }
    }
    return result;
  }

  @Override
  public QualifiedName[] getSupportedOptions() {
    return new QualifiedName[] { STYLE };
  }


  // helping methods
  //////////////////

  private static QualifiedName qualify( final String content ) {
    String pluginId = HaskellCorePlugin.getPluginId();
    return new QualifiedName( pluginId, content );
  }

  private boolean containsLatex( final String line ) {
    boolean result = false;
    for( int i = 0; !result && i < LATEX_SEQS.length; i++ ) {
      result = line.indexOf( LATEX_SEQS[ i ] ) != -1;
    }
    return result;
  }

  private BufferedReader getReader( final InputStream contents,
      final IContentDescription description )
      throws UnsupportedEncodingException {
    Reader isr;
    if( description == null || description.getCharset() == null ) {
      isr = new InputStreamReader( contents );
    } else {
      isr = new InputStreamReader( contents, description.getCharset() );
    }
    return new BufferedReader( isr );
  }
}
