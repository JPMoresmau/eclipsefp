package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.buildwrapper.types.ThingAtPoint;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.text.WordFinder;
import net.sf.eclipsefp.haskell.ui.util.text.WordFinder.EditorThing;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.hyperlink.IHyperlink;
import org.eclipse.jface.text.hyperlink.IHyperlinkDetector;


public class HaskellHyperlinkDetector implements IHyperlinkDetector {

  private final HaskellEditor editor;

  public HaskellHyperlinkDetector( final HaskellEditor editor ) {
    this.editor = editor;
  }

  @Override
  public IHyperlink[] detectHyperlinks( final ITextViewer textViewer,
      final IRegion region, final boolean canShowMultipleHyperlinks ) {
    IFile file = editor.findFile();
    Location location;
    try {
      location = new Location( file.getLocation().toOSString(),
          textViewer.getDocument(), region );
    } catch( BadLocationException ex ) {
      HaskellUIPlugin.log( UITexts.editor_textHover_error, ex );
      return null;
    }

    BWFacade f = BuildWrapperPlugin.getFacade( file.getProject() );
    if( f == null ) {
      return null;
    }

    ThingAtPoint thing = f.getThingAtPoint( file, location );
    if( thing == null ) {
      return null;
    }
    EditorThing editorThing = new EditorThing( file, thing );

    IRegion wordRegion = WordFinder
        .findWordRegion( editor.getDocument(), region.getOffset() );
    if( wordRegion == null ) {
      return null;
    }

    IHyperlink[] hyperlinks = { new HaskellHyperlink( wordRegion, editorThing,
        editor ) };
    return hyperlinks;

  }

}
