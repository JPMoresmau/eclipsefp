// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.dialog;

import java.util.HashSet;
import java.util.Set;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ElementTreeSelectionDialog;


/** <p>a dialog that allows the user to select from all folders in
  * a Haskell project, starting from a given source folder.</p>
  *
  * @author Leif Frenzel
  */
public class FolderSelectionDialog extends ElementTreeSelectionDialog {

  public FolderSelectionDialog( final Shell shell,
      final IContainer rootContainer ) {
    this(shell,rootContainer,false);
  }

  private boolean addAllowed=false;

  public FolderSelectionDialog( final Shell shell,
                                final IContainer rootContainer ,final boolean addAllowed) {
    super( shell, new DialogLabelProvider(), new FolderCP() );
    setInput( rootContainer );
    setTitle( UITexts.folder_title );
    setMessage( UITexts.folder_message );
    setEmptyListMessage( UITexts.folder_empty );
    this.addAllowed=addAllowed;
  }

  /* (non-Javadoc)
  * @see org.eclipse.ui.dialogs.ElementTreeSelectionDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
  */
  @Override
  protected Control createDialogArea( final Composite parent ) {
   Composite composite=(Composite) super.createDialogArea( parent );

   if (addAllowed){
     final Button bAdd=new Button(composite,SWT.PUSH);
     bAdd.setText( UITexts.folder_add );
     bAdd.addSelectionListener( new SelectionAdapter() {
       /* (non-Javadoc)
        * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
        */
       @Override
       public void widgetSelected( final SelectionEvent e ) {
        IStructuredSelection sel=(IStructuredSelection)getTreeViewer().getSelection();
        Object o=sel.getFirstElement();
        if (!(o instanceof IContainer)){
          o=getTreeViewer().getInput();
        }
        if (o instanceof IContainer){
          final IContainer f=(IContainer)o;
          final Set<String> names=new HashSet<>();
          try {
            for (IResource r:f.members()){
              names.add( r.getName() );
            }

            InputDialog id=new InputDialog( getShell(), UITexts.folder_add_title, UITexts.folder_add_title, "", new IInputValidator() {

             @Override
             public String isValid( final String arg0 ) {
               String s=arg0.trim();
               if (s.length()==0){
                 return UITexts.folder_add_empty;
               } else if (names.contains( s )){
                 return UITexts.folder_add_duplicate;
               }
               return null;
             }
           } );
            if (Window.OK==id.open()){
              String s=id.getValue().trim();
              IFolder f2=f.getFolder(new Path(s ));
              f2.create( false, true,new NullProgressMonitor() );
              getTreeViewer().refresh( f );
              getTreeViewer().setSelection( new StructuredSelection( f2 ) );
            }
          }catch (CoreException ce){
            HaskellUIPlugin.log( ce );
          }
        }
       }
     } );

   }

   return composite;
  }
}