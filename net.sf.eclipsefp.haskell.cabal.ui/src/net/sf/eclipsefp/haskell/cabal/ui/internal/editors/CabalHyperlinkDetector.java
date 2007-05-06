// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.ui.internal.editors;

import net.sf.eclipsefp.haskell.cabal.core.CabalSyntax;
import net.sf.eclipsefp.haskell.cabal.ui.internal.util.UITexts;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.hyperlink.IHyperlink;
import org.eclipse.jface.text.hyperlink.IHyperlinkDetector;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;

/** <p>detects places on the Cabal editor where a hyperlink can be shown that
  * leads to referenced resources.</p>
  *
  * @author Leif Frenzel
  */
class CabalHyperlinkDetector implements IHyperlinkDetector {

  private final CabalEditor editor;


  CabalHyperlinkDetector( final CabalEditor editor ) {
    this.editor = editor;
  }
  
  // interface methods of IHyperlinkDetector
  //////////////////////////////////////////
  
  public IHyperlink[] detectHyperlinks( final ITextViewer textViewer, 
                                        final IRegion region,
                                        final boolean canShowMultiple ) {
    IHyperlink[] result = null;
    String line = getLine( textViewer, region );
    if( line != null ) {
      String key = CabalSyntax.FIELD_LICENSE_FILE.toLowerCase();
      if( line.toLowerCase().startsWith( key ) ) {
        int index = line.indexOf( ':' );
        if( index > 0 ) {
          String fileName = line.substring( index + 1 ).trim();
          IPath path = new Path( fileName );
          IFile file = getContainer().getFile( path );
          if( file.exists() ) {
            int linkOffset =   getHighlightRegionStart( textViewer, region ) 
                             + line.indexOf( fileName );
            int linkLength = fileName.length();
            IRegion linkRegion = new Region( linkOffset, linkLength );
            result = new IHyperlink[] { new CabalHyperlink( file, linkRegion ) };
          }
        }
      }
    }
    return result;
  }

  
  // helping methods
  //////////////////
  
  private String getLine( final ITextViewer textViewer, 
                          final IRegion region ) {
    String result = null;
    try {
      IDocument doc = textViewer.getDocument();
      int lineNumber = doc.getLineOfOffset( region.getOffset() );
      int start = doc.getLineOffset( lineNumber );
      int length = doc.getLineLength( lineNumber );
      result = doc.get( start, length );
    } catch( BadLocationException badlox ) {
      // ignore
    }
    return result;
  }
  
  private int getHighlightRegionStart( final ITextViewer textViewer, 
                                       final IRegion region ) {
    int result = region.getOffset();
    try {
      IDocument doc = textViewer.getDocument();
      int lineNumber = doc.getLineOfOffset( region.getOffset() );
      result = doc.getLineOffset( lineNumber );
    } catch( BadLocationException badlox ) {
      // ignore
    }
    return result;
  }
  
  private IContainer getContainer() {
    IContainer result = null;
    IEditorInput input = editor.getEditorInput();
    if( input instanceof IFileEditorInput ) {
      IFileEditorInput fei = ( IFileEditorInput )input;
      IFile file = fei.getFile();
      if( file != null && file.exists() ) {
        result = file.getParent();
      }
    }
    return result;
  }
  
  
  // inner classes
  ////////////////
  
  private class CabalHyperlink implements IHyperlink {

    private final IFile file;
    private final IRegion region;

    CabalHyperlink( final IFile file, final IRegion region ) {
      this.file = file;
      this.region = region;
    }
    
    
    // interface methods of IHyperlink
    //////////////////////////////////
    
    public IRegion getHyperlinkRegion() {
      return region;
    }

    public void open() {
      IWorkbench workbench = PlatformUI.getWorkbench();
      IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
      IWorkbenchPage page = window.getActivePage();
      try {
        IDE.openEditor( page, file );
      } catch( PartInitException paix ) {
        Shell shell = window.getShell();
        String title = UITexts.cabalHyperLinkDetector_errorTitle;
        String msg = paix.getMessage();
        ErrorDialog.openError( shell, title, msg, paix.getStatus() );
      }
    }

    public String getHyperlinkText() {
      // unused
      return null;
    }

    public String getTypeLabel() {
      // unused
      return null;
    }
  }
}
