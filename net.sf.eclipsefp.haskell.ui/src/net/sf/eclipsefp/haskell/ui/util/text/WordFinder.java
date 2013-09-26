// Copyright (c) 2004-2011 by Leif Frenzel & JP Moresmau
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.util.text;

import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.JobFacade;
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.buildwrapper.types.ThingAtPoint;
import net.sf.eclipsefp.haskell.buildwrapper.types.ThingAtPointHandler;
import net.sf.eclipsefp.haskell.core.parser.ParserUtils;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;


/** <p>A helping class for detecting words in documents.</p>
  *
  * <p>Taken from
  * <code>org.eclipse.jface.text.DefaultTextDoubleClickStrategy</code>.</p>
  *
  * @author Leif Frenzel & JP Moresmau
  */
public class WordFinder {

  //private final DocumentCharacterIterator docIter = new DocumentCharacterIterator();

  public static IRegion findWordRegion( final IDocument document, final int position ) {
    if(document!=null && position >= 0 ) {
      try {
        IRegion r=document.getLineInformationOfOffset( position );
        String line=document.get( r.getOffset(), r.getLength() );
        int off=position-r.getOffset();
        IRegion lineRegion = ParserUtils.getHaskellWordRegion(line,off);
        if (lineRegion != null) {
          return new Region( r.getOffset()+lineRegion.getOffset(), lineRegion.getLength() );
        }
      } catch( final BadLocationException badlox ) {
        badlox.printStackTrace();
      }
    }
    return null;
  }

  public static String findWord( final IDocument document, final int position ) {
    IRegion r = findWordRegion( document, position );
    try {
      return document.get(r.getOffset(), r.getLength());
    } catch( BadLocationException ex ) {
      ex.printStackTrace();
    }
    return null;
  }

  public static void getEditorThing(final HaskellEditor haskellEditor,final EditorThingHandler handler){
    final IFile file = haskellEditor.findFile();
    final ISelection selection = haskellEditor.getSelectionProvider().getSelection();

    if( selection instanceof TextSelection && file!=null) {
      final TextSelection textSel = ( TextSelection )selection;
      int offset=textSel.getOffset();
      getEditorThing( haskellEditor,file, offset,handler );
    }
  }

  public static void getEditorThing(final HaskellEditor haskellEditor,final IFile file,final int offset ,final EditorThingHandler handler){
        //final ScionInstance instance = ScionPlugin.getScionInstance( file );
       JobFacade f=BuildWrapperPlugin.getJobFacade( file.getProject() );
        if (f!=null){

          //final String fName = textSel.getText().trim();
          try {
            final IDocument doc=haskellEditor.getDocument();
            Location l = new Location( file.getLocation().toOSString(),
                doc, new Region( offset, 0 ) );
            f.getThingAtPoint(file, l, new ThingAtPointHandler() {

              @Override
              public void handleThing( ThingAtPoint thing ) {
                String haddockType = null;
                /*String name=fName;
                if( thing != null && thing.length() > 0 && !"no info".equals(thing) ) {
                  name = thing;
                  if (name.startsWith("expr: ") || name.startsWith("bind:") || name.startsWith("stmt: ")){
                    name="";
                  }
                  if( name.length() > 2 && name.charAt( name.length() - 2 ) == ' ' ) {
                    haddockType = name.charAt( name.length() - 1 );
                    name = name.substring( 0, name.length() - 2 );
                  }
                }*/


                if( thing==null && doc!=null) {
                 // name = WordFinder.findWord( haskellEditor.getDocument(),
                 //     textSel.getOffset() );
                  try {
                    IRegion r=doc.getLineInformationOfOffset( offset);
                    String line=doc.get( r.getOffset(), r.getLength() );
                    int off=offset-r.getOffset();
                    String name= ParserUtils.getHaskellWord(line,off);
                    if (name!=null){
                      if (line.startsWith( "import" ) && name.contains( "." )){
                        haddockType="m";
                      }
                      thing=new ThingAtPoint( name,haddockType );
                    }
                  } catch( final BadLocationException badlox ) {
                    badlox.printStackTrace();
                  }
                }
                if (thing!=null){
                  handler.handle( new EditorThing(file, thing ));
                }
              }
            });

          } catch( BadLocationException ble ) {
            ble.printStackTrace();
          }
        }
  }

  /**
   * Simple structure wrapping a word in the editor with some useful pointers
    *
    * @author JP Moresmau
   */
  public static class EditorThing {
    private final ThingAtPoint thing;
    private final IFile file;

    public EditorThing(final IFile file, final ThingAtPoint thing ) {
      super();
      this.file=file;
      this.thing =thing;
    }



    public IFile getFile() {
      return file;
    }


    /**
     * @return the thing
     */
    public ThingAtPoint getThing() {
      return thing;
    }

  }

  /**
   * <p>Callback interface</p>
    *
    * @author JP Moresmau
   */
  public static interface EditorThingHandler{
    void handle(EditorThing thing);
  }
}
