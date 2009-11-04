// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.views.outline;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.scion.types.Location;
import net.sf.eclipsefp.haskell.scion.types.OutlineDef;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;


/** <p>The outline page for the Haskell editor.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellOutlinePage extends ContentOutlinePage {

  private List<OutlineDef> input;
  private final HaskellEditor editor;

  private Map<String,List<OutlineDef>> defByName;

  public HaskellOutlinePage( final HaskellEditor textEditor ) {
    this.editor = textEditor;
  }

  @Override
  public void createControl( final Composite parent ) {
    super.createControl( parent );

    TreeViewer viewer = getTreeViewer();
    viewer.setContentProvider( new OutlineCP() );
    viewer.setLabelProvider( new OutlineLabelProvider());
    viewer.addSelectionChangedListener( this );

    IActionBars actionBars= getSite().getActionBars();
    registerToolbarActions(actionBars);

    if( input != null ) {
      viewer.setInput( input );
    }

  }

  private void registerToolbarActions( final IActionBars actionBars ) {
    IToolBarManager toolBarManager= actionBars.getToolBarManager();
    toolBarManager.add(new LexicalSortingAction());

  }

  @Override
  public void selectionChanged( final SelectionChangedEvent event ) {
    super.selectionChanged( event );

    ISelection selection= event.getSelection();
    if( selection.isEmpty() ) {
      editor.resetHighlightRange();
    } else {
      IStructuredSelection sel = ( IStructuredSelection )selection;
      Object firstElement = sel.getFirstElement();
      /*if( firstElement instanceof IHaskellLanguageElement ) {
        IHaskellLanguageElement elem = ( IHaskellLanguageElement )firstElement;
        IEditorInput fei = editor.getEditorInput();
        IDocument doc = editor.getDocumentProvider().getDocument( fei );
        ISourceLocation srcLoc = elem.getSourceLocation();
        if( srcLoc != null ) {
          int offset = -1;
          try {
            offset = doc.getLineOffset( srcLoc.getLine() ) + srcLoc.getColumn();
          } catch( final BadLocationException badlox ) {
            // ignore
          }
          int length = elem.getName().length();
          try {
            editor.setHighlightRange( offset, length, true );
          } catch( IllegalArgumentException iaex ) {
            editor.resetHighlightRange();
          }
        }
      }*/
      if (firstElement instanceof OutlineDef){
        OutlineDef od=(OutlineDef)firstElement;
        Location srcLoc=od.getLocation();
        if (srcLoc!=null){
          IEditorInput fei = editor.getEditorInput();
          IDocument doc = editor.getDocumentProvider().getDocument( fei );
          try {
            int offset=srcLoc.getStartOffset( doc );
            int length = od.getName().length();
            try {
              editor.setHighlightRange( offset, length, true );
            } catch( IllegalArgumentException iaex ) {
              editor.resetHighlightRange();
            }
          } catch( final BadLocationException badlox ) {
            // ignore
          }

        }

      }
    }
  }


  /** <p>sets the input of the outline page.</p> */
  public void setInput( final List<OutlineDef> outlineDefs ) {
     this.input=outlineDefs;
     this.defByName=null;
     this.update();

  }


  /** <p>updates the outline page.</p> */
  public void update() {
    if (!getControl().isDisposed()){
       getControl().getDisplay().syncExec( new Runnable(){
         public void run() {
           TreeViewer viewer = getTreeViewer();
           if( viewer != null ) {
             Control control= viewer.getControl();
             if( control != null && !control.isDisposed() ) {
               control.setRedraw( false );
               viewer.setInput( input );
               viewer.expandToLevel( AbstractTreeViewer.ALL_LEVELS );
               control.setRedraw( true );
             }
           }
         }

       } );
    }
  }

  public synchronized Location getOutlineLocation(final String name){
    if (defByName==null){
      buildDefByName();
    }
    List<OutlineDef> l=defByName.get( name);
    if (l!=null && l.size()>0){
      return l.iterator().next().getLocation();
    }
    return null;
  }

  private void buildDefByName(){
    if (input!=null){
      defByName=new HashMap<String, List<OutlineDef>>();
      for (OutlineDef od:input){
        List<OutlineDef> l=defByName.get( od.getName());
        if(l==null){
          l=new ArrayList<OutlineDef>();
          defByName.put( od.getName(), l );
        }
        l.add( od );
      }
    }
  }

  class LexicalSortingAction extends Action {


    public LexicalSortingAction() {
      super();
      PlatformUI.getWorkbench().getHelpSystem().setHelp(this, "Outline.LexicalSortingAction"); //$NON-NLS-1$
      setText(UITexts.outline_sortByName);
      setImageDescriptor(HaskellUIImages.getImageDescriptor( IImageNames.ACTION_SORT ));
      setToolTipText(UITexts.outline_sortByName_tooltip);
      setDescription(UITexts.outline_sortByName_description);

      boolean checked= HaskellUIPlugin.getDefault().getPreferenceStore().getBoolean("Outline.LexicalSortingAction.isChecked"); //$NON-NLS-1$
      valueChanged(checked, false);
    }

    @Override
    public void run() {
      valueChanged(isChecked(), true);
    }

    private void valueChanged(final boolean on, final boolean store) {
      setChecked(on);
      BusyIndicator.showWhile(getTreeViewer().getControl().getDisplay(), new Runnable() {
        public void run() {
          if (on) {
            getTreeViewer().setComparator(new OutlineDefComparator( OutlineDef.BY_NAME));
          } else {
            getTreeViewer().setComparator(new OutlineDefComparator(OutlineDef.BY_LOCATION));
          }
        }
      });

      if (store) {
        HaskellUIPlugin.getDefault().getPreferenceStore().setValue("Outline.LexicalSortingAction.isChecked", on); //$NON-NLS-1$
      }
    }
  }

  class OutlineDefComparator extends ViewerComparator{
    private final Comparator<OutlineDef> comp;



    public OutlineDefComparator( final Comparator<OutlineDef> comp ) {
      super();
      this.comp = comp;
    }



    @Override
    public int compare( final Viewer viewer, final Object e1, final Object e2 ) {
     return comp.compare( (OutlineDef)e1, (OutlineDef)e2 );
    }
  }
}