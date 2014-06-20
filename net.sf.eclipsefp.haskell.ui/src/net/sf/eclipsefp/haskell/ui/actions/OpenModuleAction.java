/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.actions;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.Module;
import net.sf.eclipsefp.haskell.buildwrapper.usage.UsageAPI;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.eclipse.ui.dialogs.FilteredList;
import org.eclipse.ui.ide.IDE;


/**
 * @author JP Moresmau
 *
 */
public class OpenModuleAction extends Action implements
    IWorkbenchWindowActionDelegate {

  private Shell shell;
  private IWorkbenchPage page;
  /**
   *
   */
  public OpenModuleAction() {
    super(Platform.getResourceBundle( HaskellUIPlugin.getDefault().getBundle()).getString( "OpenModuleAction.label" ));
  }


  /* (non-Javadoc)
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  @Override
  public void run( final IAction arg0 ) {


    //elsd.setFilter( filter )
    UsageAPI api=BuildWrapperPlugin.getDefault().getUsageAPI();
    if (api!=null){
      ModuleListSelectionDialog elsd=new ModuleListSelectionDialog( shell);
      List<Module> mods=api.listLocalModules();

      elsd.setElements( mods.toArray( new Module[mods.size()] ) );
      int code=elsd.open();
      if (Window.OK==code){
        Object[] res=elsd.getResult();
        for (Object o:res){
          if (o instanceof Module){
            Module mod=(Module)o;
            Long fileid=mod.getFileID();
            if (fileid!=null){
              IFile efile=api.getFile( fileid );
              if (efile!=null){
                try {
                  IDE.openEditor( page, efile);
                } catch (PartInitException pie){
                  HaskellUIPlugin.log( pie );
                }
              }
            }
          }
        }
      }
    }
  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void selectionChanged( final IAction arg0, final ISelection arg1 ) {
    //NOOP
  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
   */
  @Override
  public void dispose() {
   shell=null;
   page=null;

  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#init(org.eclipse.ui.IWorkbenchWindow)
   */
  @Override
  public void init( final IWorkbenchWindow arg0 ) {
    shell=arg0.getShell();
    page=arg0.getActivePage();
  }

  private static class ModuleListSelectionDialog extends ElementListSelectionDialog {
    /**
     * table items that have the selection focus (show package name)
     */
    private final Map<Integer,TableItem> lastTis=new HashMap<>();

    public ModuleListSelectionDialog( final Shell parent ) {
      super( parent, new ModuleLabelProvider() );
      setTitle( UITexts.OpenModuleAction_title );
      setMessage( UITexts.OpenModuleAction_text );
      setEmptyListMessage( UITexts.OpenModuleAction_nomodule );
      setEmptySelectionMessage( UITexts.OpenModuleAction_nomodulesel );
      setMultipleSelection( true );
      setStatusLineAboveButtons( true );
      setAllowDuplicates( true );
    }

    /* (non-Javadoc)
     * @see org.eclipse.ui.dialogs.SelectionStatusDialog#configureShell(org.eclipse.swt.widgets.Shell)
     */
    @Override
    protected void configureShell( final Shell shell ) {
      super.configureShell( shell );
      shell.setImage( HaskellUIImages.getImage( IImageNames.HASKELL_MISC ));
    }

    /* (non-Javadoc)
     * @see org.eclipse.ui.dialogs.AbstractElementListSelectionDialog#createFilteredList(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected FilteredList createFilteredList( final Composite parent ) {
      FilteredList fl=super.createFilteredList( parent );
      fl.addSelectionListener( new SelectionAdapter() {
        @Override
        public void widgetSelected(final SelectionEvent e) {
            TableItem[] tis=((Table)e.widget).getItems();
            Object[] obj=getSelectedElements();
            int[] sels=getSelectionIndices();
            /**
             * new focused table items
             */
            Map<Integer,TableItem> newTis=new HashMap<>();
            for (int a=0;a<sels.length;a++){
              int idx=sels[a];
              Module mod=(Module)obj[a];

              TableItem ti=lastTis.remove( idx );
              // change text
              if (ti==null){
                tis[idx].setText( mod.getModuleName() +" - "+ mod.getPackageName() );
              }
              newTis.put( idx, tis[idx] );
            }
            // reset text to unselected
            for (Integer i:lastTis.keySet()){
              TableItem ti=lastTis.get(i);
              if (ti!=null && !ti.isDisposed()){
                String s=ti.getText();
                int ix=s.indexOf( " - " );
                if (ix>-1){
                  ti.setText( s.substring( 0,ix ) );
                }
              }
            }
            lastTis.clear();
            // new map
            lastTis.putAll( newTis );

        }
      } );
      return fl;
    }

  }

  private static class ModuleLabelProvider extends LabelProvider{
    private final Image img;
    /**
     *
     */
    public ModuleLabelProvider() {
      img=HaskellUIImages.getImage( IImageNames.MODULE );
    }

    @Override
    public String getText(final Object element) {
      if (element instanceof Module){
        Module mod=(Module)element;
        return mod.getModuleName();
      }
      return super.getText( element );
    }

    @Override
    public org.eclipse.swt.graphics.Image getImage(final Object element) {
      if (element instanceof Module){
        return img;
      }
      return super.getImage( element );

    }



  }
}
