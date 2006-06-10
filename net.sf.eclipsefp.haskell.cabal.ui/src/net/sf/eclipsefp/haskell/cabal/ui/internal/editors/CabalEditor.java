// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.ui.internal.editors;

import net.sf.eclipsefp.haskell.cabal.core.model.PackageDescription;
import net.sf.eclipsefp.haskell.cabal.core.model.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.cabal.ui.internal.editors.outline.CabalOutlinePage;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.jface.text.source.projection.ProjectionSupport;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

/** <p>an editor for Cabal package description files.</p> 
  * 
  * <p>Note: this class is declared in <code>plugin.xml</code>.</p>
  *
  * @author Leif Frenzel
  */
public class CabalEditor extends TextEditor {

  private CabalOutlinePage outlinePage;
  private ProjectionSupport projectionSupport;
  private PackageDescription packageDescription;
  
  public CabalEditor() {
    setSourceViewerConfiguration( new CabalConfiguration( this ) );
  }
  
  public void setPackageDescription( final PackageDescription packageDescription ) {
    this.packageDescription = packageDescription;
    if( outlinePage != null ) {
      outlinePage.setPackageDescription( packageDescription );
    }
  }

  public void selectAndReveal( final Object element ) {
    if( element instanceof PackageDescriptionStanza ) {
      PackageDescriptionStanza stanza = (PackageDescriptionStanza) element;
      IDocument doc = getSourceViewer().getDocument();
      try {
        int offset = doc.getLineOffset( stanza.getStartLine() );
        int end = doc.getLength();
        try {
          end =   doc.getLineOffset( stanza.getEndLine() ) 
                + doc.getLineLength( stanza.getEndLine() );
        } catch( final BadLocationException badlox ) {
          // ignore
        }
        int length = end - offset;
        selectAndReveal( offset, length );
      } catch( final BadLocationException badlox ) {
        // ignore
      }
    }
  }

  // interface methods of TextEditor
  //////////////////////////////////
  
  protected ISourceViewer createSourceViewer( final Composite parent, 
                                              final IVerticalRuler ruler, 
                                              final int styles ) {
    fOverviewRuler = createOverviewRuler( getSharedColors() );
    ISourceViewer viewer 
      = new ProjectionViewer( parent, ruler, fOverviewRuler, true, styles );
    // ensure decoration support has been created and configured:
    getSourceViewerDecorationSupport( viewer );
    return viewer;
  }
  
  public void createPartControl( final Composite parent ) {
    super.createPartControl( parent );

    ProjectionViewer pv = ( ProjectionViewer )getSourceViewer();
    projectionSupport = new ProjectionSupport( pv, 
                                               getAnnotationAccess(), 
                                               getSharedColors() );
    projectionSupport.install();
    pv.doOperation( ProjectionViewer.TOGGLE );
  }
  
  
  // interface methods of IAdaptable
  //////////////////////////////////
  
  public Object getAdapter( final Class adapterType ) {
    Object result = null;
    if( IContentOutlinePage.class.equals( adapterType ) ) {
      if( outlinePage == null ) {
        outlinePage = new CabalOutlinePage( this, packageDescription );
      }
      result = outlinePage;
    } else if( projectionSupport != null ) {
      Object adapter
        = projectionSupport.getAdapter( getSourceViewer(), adapterType );
      if( adapter != null ) {
        result = adapter;
      }
    }

    if( result == null ) {
      result = super.getAdapter( adapterType );
    }
    return result;
  }
}
