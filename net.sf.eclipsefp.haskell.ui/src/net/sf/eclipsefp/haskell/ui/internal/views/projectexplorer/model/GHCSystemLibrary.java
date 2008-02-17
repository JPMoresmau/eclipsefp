// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer.model;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerManager;
import net.sf.eclipsefp.haskell.core.internal.hsimpl.IHsImplementation;
import net.sf.eclipsefp.haskell.core.util.QueryUtil;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import de.leiffrenzel.cohatoe.server.core.data.Pair;
import de.leiffrenzel.cohatoe.server.core.util.GHCSyntax;


public class GHCSystemLibrary implements ITreeElement {

  private final IProject project;

  public GHCSystemLibrary( final IProject project ) {
    this.project = project;
  }


  // interface methods of ITreeElement
  ////////////////////////////////////

  public List<GHCPackageConf> getChildren() {
    List<GHCPackageConf> result = new ArrayList<GHCPackageConf>();
    ICompilerManager man = CompilerManager.getInstance();
    if( man.getCurrentHsImplementation() != null ) {
      IPath binDir = new Path( man.getCurrentHsImplementation().getBinDir() );
      String exe =  binDir.append( GHCSyntax.GHC_PKG ).toOSString();
      String queryResult = QueryUtil.query( exe, "list" );
      parsePackageList( queryResult, result );
    }
    return result;
  }

  public Object getParent() {
    return project;
  }

  public String getText() {
    ICompilerManager man = CompilerManager.getInstance();
    IHsImplementation impl = man.getCurrentHsImplementation();
    String name = "No Haskell implementation configured!";
    if( impl != null ) {
      name = impl.getName();
    }
    return "GHC Libraries [" + name + "]";
  }

  public String getImageKey() {
    return IImageNames.IMPORT_LIBRARY;
  }


  // helping functions
  ////////////////////

  private void parsePackageList( final String content,
                                 final List<GHCPackageConf> confs ) {
    ArrayList<Pair<String, StringBuilder>> entries
      = new ArrayList<Pair<String,StringBuilder>>();
    try {
      BufferedReader br = new BufferedReader( new StringReader( content ) );
      Pair<String, StringBuilder> entry = null;
      String line = br.readLine();
      while( line != null ) {
        if( line.startsWith( "  " ) && entry != null ) {
          entry.snd().append(  line.trim() );
        } else {
          entry = new Pair<String, StringBuilder>( line, new StringBuilder() );
          entries.add( entry );
        }
        line = br.readLine();
      }
    } catch( final IOException ioex ) {
      // won't happen, we're just reading a string
    }
    for( Pair<String, StringBuilder> entry: entries ) {
      Path loc = new Path( entry.fst().trim() );
      confs.add( new GHCPackageConf( this, loc, entry.snd().toString() ) );
    }
  }
}
