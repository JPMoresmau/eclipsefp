/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.search;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import net.sf.eclipsefp.haskell.buildwrapper.usage.UsageQueryFlags;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.search.ui.ISearchPage;
import org.eclipse.search.ui.ISearchPageContainer;
import org.eclipse.search.ui.ISearchResultViewPart;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkingSet;
import org.eclipse.ui.progress.UIJob;


/**
 * @author JP Moresmau
 *
 */
public class HaskellSearchPage extends DialogPage implements ISearchPage{
  private static LinkedList<String> previous=new LinkedList<String>();
  private ISearchPageContainer container;

  private ComboViewer termList;

  private final Map<Button,Integer> buttonsToType=new HashMap<Button, Integer>();
  private final Map<Button,Integer> buttonsToScope=new HashMap<Button, Integer>();

  public HaskellSearchPage() {
    super();
  }

  public HaskellSearchPage( final String title, final ImageDescriptor image ) {
    super( title, image );
  }

  public HaskellSearchPage( final String title ) {
    super( title );
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite arg0 ) {
    Composite main=new Composite(arg0,SWT.NONE);
    main.setLayout( new GridLayout(2,false) );
    Label l=new Label(main,SWT.NONE);
    l.setText( UITexts.SearchPage_text );
    GridData gd=new GridData( GridData.HORIZONTAL_ALIGN_FILL );
    gd.horizontalSpan=2;
    l.setLayoutData( gd );

    termList=new ComboViewer( main,SWT.NONE );
    gd=new GridData( GridData.HORIZONTAL_ALIGN_FILL );
    gd.horizontalSpan=2;
    termList.getCombo().setLayoutData( gd );
    termList.setContentProvider( new ArrayContentProvider() );
    termList.setInput( previous );


    Group gType=new Group(main,SWT.NONE);
    gType.setText( UITexts.SearchPage_type );
    gType.setLayout( new GridLayout(2,true) );
    final Button bModule=createTypeButton(gType,UsageQueryFlags.TYPE_MODULE,UITexts.SearchPage_type_modules,false);
    createTypeButton(gType,UsageQueryFlags.TYPE_TYPE,UITexts.SearchPage_type_types,false);
    createTypeButton(gType,UsageQueryFlags.TYPE_CONSTRUCTOR,UITexts.SearchPage_type_constructors,false);
    final Button bFunctions=createTypeButton(gType,UsageQueryFlags.TYPE_VAR,UITexts.SearchPage_type_functions,true);

    Group gScope=new Group(main,SWT.NONE);
    gScope.setText( UITexts.SearchPage_scope );
    gScope.setLayout( new GridLayout(2,true) );
    final Button bAll=createScopeButton( gScope, UsageQueryFlags.SCOPE_ALL, UITexts.SearchPage_scope_all, true );
    createScopeButton( gScope, UsageQueryFlags.SCOPE_DEFINITIONS, UITexts.SearchPage_scope_declarations, false );
    final Button bRefs=createScopeButton( gScope, UsageQueryFlags.SCOPE_REFERENCES, UITexts.SearchPage_scope_references, false );
    setControl( main );

    if (container.getSelection() instanceof IStructuredSelection){
      Object o=((IStructuredSelection)container.getSelection()).getFirstElement();
      if (o instanceof IFile){
        String module=ResourceUtil.getModuleName( (IFile )o);
        if (module!=null && module.length()>0){
          termList.getCombo().setText( module );
          bModule.setSelection( true );
          bFunctions.setSelection( false );
          bModule.notifyListeners( SWT.Selection, new Event() );
          bRefs.setSelection( true );
          bAll.setSelection( false );
          bRefs.notifyListeners( SWT.Selection, new Event() );
        }
      }
    }

    setErrorMessage( null );
  }

  private Button createTypeButton(final Group gType,final int type,final String text,final boolean selected){
    Button b=new Button(gType,SWT.RADIO);
    b.setText( text );
    buttonsToType.put( b, type );
    b.setSelection( selected );
    return b;
  }

  private Button createScopeButton(final Group gScope,final int type,final String text,final boolean selected){
    Button b=new Button(gScope,SWT.RADIO);
    b.setText( text );
    buttonsToScope.put( b, type );
    b.setSelection( selected );
    return b;
  }

  /* (non-Javadoc)
   * @see org.eclipse.search.ui.ISearchPage#performAction()
   */
  @Override
  public boolean performAction() {
    String term=termList.getCombo().getText();
    if (term.length()>0){
      previous.remove( term );
      previous.addFirst( term );

      String[] projects=container.getSelectedProjectNames();
      IWorkingSet[] sets=container.getSelectedWorkingSets();

      Set<IProject> projs=new HashSet<IProject>();
      Set<IResource> otherResources=new HashSet<IResource>();
      if (projects!=null){
        IWorkspaceRoot root=ResourcesPlugin.getWorkspace().getRoot();
        for (String pn:projects){
          IProject p=root.getProject( pn );
          if (p!=null){
            projs.add(p);
          }
        }
      } else if (sets!=null) {
        for (IWorkingSet set:sets){
          for (IAdaptable adp:set.getElements()){
            if (adp instanceof IProject){
              projs.add(( IProject )adp);
            } else if (adp instanceof IResource){
              otherResources.add( (IResource )adp);
            } else {
              Object o=adp.getAdapter( IResource.class );
              if (o instanceof IProject){
                projs.add(( IProject )o);
              } else if (o instanceof IResource){
                otherResources.add( (IResource )o);
              }
            }
          }
        }
      } else if (container.getSelectedScope()==ISearchPageContainer.SELECTION_SCOPE){
        ISelection sel=container.getSelection();
        if (sel instanceof IStructuredSelection){
          for (Iterator<?> it=((IStructuredSelection)sel).iterator();it.hasNext();){
            Object o=it.next();
            if (o instanceof IProject){
              projs.add(( IProject )o);
            } else if (o instanceof IResource){
              otherResources.add( (IResource )o);
            }
          }
        }
      }

      final UsageQuery uq=new UsageQuery( term, projs );
      uq.setExact( false );
      for (Map.Entry<Button,Integer> me:buttonsToType.entrySet()){
        if (me.getKey().getSelection()){
          uq.setTypeFlags( me.getValue() );
        }
      }
      for (Map.Entry<Button,Integer> me:buttonsToScope.entrySet()){
        if (me.getKey().getSelection()){
          uq.setScopeFlags( me.getValue() );
        }
      }
      if (otherResources.size()>0){
        uq.setRestrictedResources( otherResources );
      }
      new UIJob( UITexts.openDefinition_select_job ) {

        @Override
        public IStatus runInUIThread( final IProgressMonitor monitor ) {
          final ISearchResultViewPart p=NewSearchUI.activateSearchResultView();
          NewSearchUI.runQueryInBackground( uq,p);
          return Status.OK_STATUS;
        }
      }.schedule();
      return true;
    }

    return false;
  }

  /* (non-Javadoc)
   * @see org.eclipse.search.ui.ISearchPage#setContainer(org.eclipse.search.ui.ISearchPageContainer)
   */
  @Override
  public void setContainer( final ISearchPageContainer paramISearchPageContainer ) {
    this.container=paramISearchPageContainer;
    //this.container.setActiveEditorCanProvideScopeSelection( this.container.getActiveEditorInput() instanceof FileEditorInput );
    //this.container.setActiveEditorCanProvideScopeSelection(false);
  }

}
