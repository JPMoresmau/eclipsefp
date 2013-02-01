// Copyright (c) 2004-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import java.io.IOException;
import java.net.URL;
import java.util.Iterator;
import net.sf.eclipsefp.haskell.browser.items.Documented;
import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.buildwrapper.types.ThingAtPoint;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports.ImportsManager;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DefaultTextHover;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextHoverExtension;
import org.eclipse.jface.text.ITextHoverExtension2;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationAccessExtension;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.texteditor.DefaultMarkerAnnotationAccess;

public class HaskellTextHover extends DefaultTextHover implements ITextHoverExtension, ITextHoverExtension2 {

  private final HaskellEditor editor;

  private static final String ERROR_ANNOTATION_TYPE = "org.eclipse.ui.workbench.texteditor.error"; //$NON-NLS-1$
  private static final String WARNING_ANNOTATION_TYPE = "org.eclipse.ui.workbench.texteditor.warning"; //$NON-NLS-1$

  private final DefaultMarkerAnnotationAccess fMarkerAnnotationAccess;

  HaskellTextHover( final HaskellEditor editor,
                    final ISourceViewer sourceViewer ) {
    super( sourceViewer );
    this.editor = editor;
    fMarkerAnnotationAccess = new DefaultMarkerAnnotationAccess();
  }

  @Override
  public String getHoverInfo( final ITextViewer textViewer,
                              final IRegion hoverRegion ) {
    // TODO TtC this method does not get called when hovering over a string literal
    // which leads to potential problem annotation hovers not appearing
    String hoverInfo = computeProblemInfo( textViewer, hoverRegion,fMarkerAnnotationAccess);
    if (hoverInfo != null) {
      return hoverInfo;
    }
    return computeThingAtPoint( textViewer, hoverRegion,false );
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.text.ITextHoverExtension2#getHoverInfo2(org.eclipse.jface.text.ITextViewer, org.eclipse.jface.text.IRegion)
   */
  @Override
  public Object getHoverInfo2( final ITextViewer textViewer,
      final IRegion hoverRegion) {
    String hoverInfo = computeProblemInfo( textViewer, hoverRegion,fMarkerAnnotationAccess);
    String tap = computeThingAtPoint( textViewer, hoverRegion,true );
    if (tap==null){
      return hoverInfo;
    } else if (hoverInfo==null){
      return tap;
    }
    // combine markers and thing at point
    return hoverInfo+"<hr>"+tap;
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.text.ITextHoverExtension#getHoverControlCreator()
   */
  @Override
  public IInformationControlCreator getHoverControlCreator() {
    return new IInformationControlCreator() {

      @Override
      public IInformationControl createInformationControl( final Shell parent ) {
        return new HaskellInformationControl(parent);
        //return new DefaultInformationControl( parent, false ); // for bold-only tooltips
      }
    } ;
  }

  private static String toHTMLString(final String txt){
    String txt2=txt.replace( PlatformUtil.NL, "<br/>" );
    txt2=txt2.replace( "\n", "<br/>" );
    txt2=txt2.replace( "\r", "<br/>" );
    txt2=txt2.replace( " ", "&nbsp;" );
    txt2="<nobr>"+txt2.replace("<br/>","</nobr><br/><nobr>")+"</nobr>";
    return txt2;
  }

  private static String toHTMLString(final String txt,final boolean html){
    if (html){
      return toHTMLString( txt );
    }
    return txt;
  }

  @SuppressWarnings ( "unchecked" )
  public static String computeProblemInfo( final ITextViewer textViewer, final IRegion hoverRegion,final IAnnotationAccessExtension  fMarkerAnnotationAccess) {
    if (textViewer instanceof ISourceViewer) {
      IAnnotationModel annotationModel = ((ISourceViewer)textViewer).getAnnotationModel();
      if (annotationModel!=null){
        Iterator<Annotation> i = annotationModel.getAnnotationIterator();
        while (i.hasNext()) {
          Annotation a = i.next();
          String type = a.getType();
          if (a.getText()!=null && (fMarkerAnnotationAccess.isSubtype( type, ERROR_ANNOTATION_TYPE ) ||
              fMarkerAnnotationAccess.isSubtype( type, WARNING_ANNOTATION_TYPE ))) {
            Position p = annotationModel.getPosition( a );
            if (p.overlapsWith( hoverRegion.getOffset(), hoverRegion.getLength() )) {
              // add a nice icon
              String img="";
              String txt=toHTMLString(a.getText());

              try {
                URL url =FileLocator.toFileURL( HaskellUIPlugin.getDefault().getBundle().getResource(
                    fMarkerAnnotationAccess.isSubtype( type, ERROR_ANNOTATION_TYPE )?"icons/obj16/error_obj.gif":"icons/obj16/warning_obj.gif"));
                img="<img src=\"" + url.toString()+"\" style=\"vertical-align:-4;\"/>";
              } catch( IOException ioe){
                HaskellUIPlugin.log( ioe );
              }

              return "<div style=\"font-family: verdana;padding:2px\">"+img+
                     txt +
                     "</div>";
            }
          }
        }
      }
    }
    return null;
  }

  protected String computeThingAtPoint( final ITextViewer textViewer, final IRegion hoverRegion,final boolean html  ) {
    if (editor==null){
      return null;
    }
    IFile file = editor.findFile();
    if (file != null) {
      try {
        Location location = new Location(file.getLocation().toOSString(), textViewer.getDocument(), hoverRegion);
        //IDocument theDocument = textViewer.getDocument();
        //ScionInstance scionInstance = ScionPlugin.getScionInstance( file );
        BWFacade f=BuildWrapperPlugin.getFacade( file.getProject() );
        if (f != null) {
          //long t0=System.currentTimeMillis();
          //try {
            ThingAtPoint tap=f.getThingAtPoint(file,location);
            if (tap!=null){
              StringBuilder sb=new StringBuilder();


              sb.append(html ? "<div style='font-family: verdana; padding:2px'><nobr>" : "");
              sb.append(tap.getName());
              String moduleColor = HaskellUIPlugin.getDefault().getPreferenceStore().getString( IEditorPreferenceNames.EDITOR_CON_COLOR );

              if (tap.getType()!=null){
                sb.append(" :: ");
                sb.append(html ? "<b>" : "");
                sb.append(tap.getType());
                sb.append(html ? "</b>" : "");
              }
              sb.append(html ? "</nobr>" : "");
              if (tap.getModule()!=null){
                sb.append(html ? "<br/>" : "\n");
                sb.append(html ? "<span style='color: grey'>"+UITexts.editor_textHover_module+"</span>&nbsp;<span style='color:rgb("+moduleColor+"); font-weight: bold'><nobr>" : "");
                sb.append(tap.getModule());
                sb.append(html ? "</nobr></span>" : "");
              }
              sb.append(html ? "</div>" : "");

              ImportsManager im=editor.getImportsManager();
              Documented d=im.getDeclarations().get( tap.getName() );
              if (d!=null && d.getDoc()!=null && d.getDoc().length()>0){
                sb.append(html ? "<hr/>" : "\n");
                sb.append(html ? "<div style='font-family: verdana; padding:2px'>" : "");
                sb.append(toHTMLString(d.getDoc(),html));
                sb.append(html ? "</div>" : "");

              }

              return sb.toString();
            }
          //} finally {
          //  long t1=System.currentTimeMillis();
          //  HaskellUIPlugin.log( "computethingAtPoint: "+(t1-t0)+"ms", IStatus.INFO );
          //}
        }
      } catch (BadLocationException ex) {
        HaskellUIPlugin.log( UITexts.editor_textHover_error, ex );
      }
    }
    return null;
  }

  @Override
  protected boolean isIncluded( final Annotation annotation ) {
    return false;
  }




//  private class ComputeThingAtPoint extends FileCommandGroup {
//    private String thing;
//    private final ITextViewer textViewer;
//    private final IFile theFile;
//    private final IRegion hoverRegion;
//
//    public ComputeThingAtPoint( final String jobName, final IFile theFile, final ITextViewer textviewer, final IRegion hoverRegion ) {
//      super( jobName, theFile, Job.SHORT );
//      this.thing = null;
//      this.textViewer = textviewer;
//      this.theFile = theFile;
//      this.hoverRegion = hoverRegion;
//    }
//
//    @Override
//    protected IStatus run( final IProgressMonitor monitor ) {
//      IDocument document = textViewer.getDocument();
//      Location location;
//      try {
//        location = new Location(theFile.getLocation().toOSString(), document, hoverRegion);
//        ScionInstance scionInstance = ScionPlugin.getScionInstance( theFile );
//        if (scionInstance != null) {
//          thing = scionInstance.thingAtPoint(document,location, false, true);
//        }
//      } catch (BadLocationException ex) {
//        HaskellUIPlugin.log( UITexts.editor_textHover_error, ex );
//      }
//
//      return Status.OK_STATUS;
//    }
//
//    public String getThing() {
//      return thing;
//    }
//  }
}