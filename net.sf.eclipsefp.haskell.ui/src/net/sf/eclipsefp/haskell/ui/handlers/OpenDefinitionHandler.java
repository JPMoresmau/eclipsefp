package net.sf.eclipsefp.haskell.ui.handlers;

import java.io.File;
import java.net.URI;
import net.sf.eclipsefp.haskell.core.parser.ParserUtils;
import net.sf.eclipsefp.haskell.scion.types.Location;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.ITextEditor;

/**
 * Action that opens an editor and scrolls it to the definition of the currently selected element.
 *
 * @author Thomas ten Cate
 */
public class OpenDefinitionHandler extends AbstractHandler {

	public OpenDefinitionHandler() {
		// explicit default constructor
	}

	public Object execute(final ExecutionEvent event) {
		IEditorPart editor = HandlerUtil.getActiveEditor(event);
		if (editor instanceof HaskellEditor) {
			HaskellEditor haskellEditor = (HaskellEditor)editor;
			IFile file = haskellEditor.findFile();
			ISelection selection = haskellEditor.getSelectionProvider().getSelection();
			if (selection instanceof TextSelection) {
				TextSelection textSel = (TextSelection)selection;
				String name = textSel.getText().trim();
				if (name.length()==0){
				  try {

  				  Location l=new Location(file.getLocation().toOSString(),haskellEditor.getDocument(), new Region(textSel.getOffset(),0));
  				  String s=HaskellUIPlugin.getDefault().getScionInstanceManager( file ).thingAtPoint( l );
  				  if (s!=null && s.length()>0){
  				    name=s;
  				    int ix=name.indexOf( ' ' );
  				    if (ix>-1){
  				      name=name.substring( 0,ix );
  				    }
  				    ix=name.lastIndexOf( '.' );
              if (ix>-1){
                name=name.substring( ix+1 );
              }
  				  }

				  } catch(BadLocationException ble){
				    ble.printStackTrace();
				  }

				}
				if (name.length()==0){
          try {
            IRegion r=haskellEditor.getDocument().getLineInformationOfOffset( textSel.getOffset() );
            String line=haskellEditor.getDocument().get( r.getOffset(), r.getLength() );
            int off=textSel.getOffset()-r.getOffset();
            name=ParserUtils.getHaskellWord(line,off);
          } catch(BadLocationException ble){
            ble.printStackTrace();
          }
				}
				if (name.length()>0){
  				Location location = HaskellUIPlugin.getDefault().getScionInstanceManager( file ).firstDefinitionLocation(name);
  				if (location != null) {
  					try {
  						openInEditor(haskellEditor.getEditorSite().getPage(), location,file.getProject());
  					} catch (PartInitException ex) {
  					  ex.printStackTrace();
  						// too bad
  					}
  				}
				}
			}
		}
		return null;
	}

	protected void openInEditor(final IWorkbenchPage page, final Location location,final IProject p) throws PartInitException {
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IWorkspaceRoot root = workspace.getRoot();
		URI uri = new File(location.getFileName()).toURI(); //new URI("file", "", location.getFileName(), null, null);
		IFile[] files = root.findFilesForLocationURI(uri, IResource.FILE);
		if (files.length > 0) {
		  IFile file = files[0]; // open only the first file; they should be the same anyway

		  if (!file.getProject().equals( p ) && files.length>1){
		    for (IFile f:files){
		      if (f.getProject().equals( p )){
		        file=f;
		        break;
		      }
		    }
		  }
			IEditorPart editor = IDE.openEditor(page, file, true);
			ITextEditor textEditor = (ITextEditor)editor;
			IDocument document = textEditor.getDocumentProvider().getDocument(editor.getEditorInput());
			try {
				int startOffset = location.getStartOffset(document);
				int length = location.getLength(document);
				textEditor.selectAndReveal(startOffset, length);
			} catch (BadLocationException ex) {
				// ignore
			}
		}
	}

}
