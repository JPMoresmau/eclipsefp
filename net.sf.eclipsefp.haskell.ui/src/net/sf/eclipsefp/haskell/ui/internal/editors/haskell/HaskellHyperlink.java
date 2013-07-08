package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import net.sf.eclipsefp.haskell.ui.handlers.OpenDefinitionHandler;
import net.sf.eclipsefp.haskell.ui.util.text.WordFinder.EditorThing;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.hyperlink.IHyperlink;


public class HaskellHyperlink implements IHyperlink {

  private final IRegion region;
  private final EditorThing thing;
  private final HaskellEditor haskellEditor;

  public HaskellHyperlink( final IRegion region, final EditorThing thing,
      final HaskellEditor haskellEditor ) {
    this.region = region;
    this.thing = thing;
    this.haskellEditor = haskellEditor;
  }

  @Override
  public IRegion getHyperlinkRegion() {
    return region;
  }

  @Override
  public String getTypeLabel() {
    return null;
  }

  @Override
  public String getHyperlinkText() {
    return "Open Declaration";
  }

  @Override
  public void open() {
    OpenDefinitionHandler.openDefinition( haskellEditor, thing );
  }

}
