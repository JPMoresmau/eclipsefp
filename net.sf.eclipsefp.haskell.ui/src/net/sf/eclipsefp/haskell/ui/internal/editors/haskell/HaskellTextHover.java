// Copyright (c) 2004-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.scion.client.Scion;
import net.sf.eclipsefp.haskell.scion.commands.ThingAtPointCommand;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.SrcLoc;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DefaultTextHover;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.ISourceViewer;

class HaskellTextHover extends DefaultTextHover {

  private final HaskellEditor editor;

  HaskellTextHover( final HaskellEditor editor,
                    final ISourceViewer sourceViewer ) {
    super( sourceViewer );
    this.editor = editor;
  }

  @Override
  public String getHoverInfo( final ITextViewer textViewer,
                              final IRegion hoverRegion ) {
	  /*
    String result = null;
    CohatoeServer server = CohatoeServer.getInstance();
    IEditorTextHover fun = server.createFunction( IEditorTextHover.class );
    if( fun != null ) {
      try {
        IDocument doc = textViewer.getDocument();
        SrcLoc loc = SrcLoc.fromDocOffset( doc, hoverRegion.getOffset() );
        IFile file = findFile();
        if( file != null ) {
          IFile cabalFile = getCabalFile( file.getProject() );
          result = fun.computeInfoHover( cabalFile, file, loc.getLine(), loc.getColumn() );
        }
      } catch( BadLocationException ex ) {
        // ignore, won't get any hover then
      }
    }
    return result;
    */
	  IFile file = editor.findFile();
	  if (file != null) {
		  IDocument doc = textViewer.getDocument();
		  try {
			  SrcLoc loc = SrcLoc.fromDocOffset(doc, hoverRegion.getOffset());
			  ThingAtPointCommand command = new ThingAtPointCommand(file.getLocation().toOSString(), loc.getLine(), loc.getColumn());
			  Scion.syncRunCommand(command, 200);
			  if (command.isSuccessful()) {
				  return command.getThing();
			  }
			  return null;
		  } catch (BadLocationException ex) {
			  // TODO Auto-generated catch block
			  ex.printStackTrace();
		  }
	  }
	  return null;
  }

  @Override
  protected boolean isIncluded( final Annotation annotation ) {
    return false;
  }

  private IFile getCabalFile( final IProject project ) {
    String ext = ResourceUtil.EXTENSION_CABAL;
    IPath path = new Path( project.getName() ).addFileExtension( ext );
    return project.getFile( path );
  }
}