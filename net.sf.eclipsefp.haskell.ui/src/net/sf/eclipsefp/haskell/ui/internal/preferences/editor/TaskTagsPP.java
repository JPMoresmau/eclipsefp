/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.preferences.editor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.common.ui.dialog.DialogField;
import net.sf.eclipsefp.common.ui.preferences.overlay.OverlayPreferenceStore;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.TaskTag;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;


/**
 * Preference page for task tags
 * @author JP Moresmau
 *
 */
public class TaskTagsPP extends AbstractEditorPP {
  private final Set<TaskTag> tags=new HashSet<TaskTag>();
  private TableViewer tableViewer;
  private Table tTags;

  /**
   * empty
   */
  public TaskTagsPP() {

  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.ui.internal.preferences.editor.AbstractEditorPP#addPreferences(net.sf.eclipsefp.common.ui.preferences.overlay.OverlayPreferenceStore)
   */
  @Override
  protected void addPreferences( final OverlayPreferenceStore store ) {
    store.addStringKey( EDITOR_TASK_TAGS );
    store.addStringKey( EDITOR_TASK_TAGS_CASE );
  }


  /* (non-Javadoc)
   * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( final Composite parent ) {

    Composite control = new Composite( parent, SWT.NONE );
    GridLayout layout = new GridLayout(2,false);
    control.setLayout( layout );

    Label lText=new Label( control, SWT.NONE );
    lText.setText( UITexts.tasks_pref_text );
    GridData gdText=new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL);
    gdText.horizontalSpan=2;
    lText.setLayoutData( gdText );

    tTags=new Table( control, SWT.V_SCROLL | SWT.SINGLE );
    GridData gdTable=new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.VERTICAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL);
    tTags.setLayoutData( gdTable );

    tTags.setHeaderVisible( true );
    tTags.setLinesVisible( true );


    TableColumn tcTag=new TableColumn( tTags, SWT.LEFT );
    tcTag.setText( UITexts.tasks_pref_tag );
    tcTag.setWidth(100);

    TableColumn tcPriority=new TableColumn( tTags, SWT.LEFT );
    tcPriority.setText( UITexts.tasks_pref_priority );
    tcPriority.setWidth(100);

    tTags.setSortColumn( tcTag );
    tTags.setSortDirection( SWT.UP );

    SelectionAdapter sel= new SelectionAdapter() {
      @Override
      public void widgetSelected(final SelectionEvent e) {
        if (e.widget==tTags.getSortColumn()){
          tTags.setSortDirection( tTags.getSortDirection()==SWT.UP?SWT.DOWN:SWT.UP );
        } else {
          tTags.setSortColumn( (TableColumn )e.widget);
        }
        update();
      }

    };
    tcTag.addSelectionListener( sel );
    tcPriority.addSelectionListener( sel );

    tableViewer=new TableViewer( tTags );
    tableViewer.setUseHashlookup( true );
    tableViewer.setLabelProvider( new TaskTagLabelProvider() );
    //tableViewer.setCellModifier( new TaskTagModifier() );
    tableViewer.setColumnProperties( new String[]{"tag","priority"} );
    tableViewer.setContentProvider( new ArrayContentProvider() );
    //tableViewer.setComparator( new WorkbenchViewerComparator() );

    Composite cButtons=new Composite(control,SWT.NONE);
    GridLayout rl=new GridLayout( 1,true ) ;
    cButtons.setLayout(rl);
    cButtons.setLayoutData( new GridData(GridData.VERTICAL_ALIGN_BEGINNING) );

    Button bNew=new Button(cButtons,SWT.PUSH);
    bNew.setText( UITexts.generic_new );
    bNew.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_FILL) );
    bNew.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected(final SelectionEvent e) {
        TaskTagDialog ttd=new TaskTagDialog( getShell() );
        if (ttd.open()==Window.OK){
          tags.add( ttd.getTaskTag() );
          update();
        }
      }
    } );

    final Button bEdit=new Button(cButtons,SWT.PUSH);
    bEdit.setText( UITexts.generic_edit );
    bEdit.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_FILL) );
    bEdit.setEnabled( false );
    bEdit.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected(final SelectionEvent e) {
        TaskTag tt=(TaskTag)((IStructuredSelection)tableViewer.getSelection()).getFirstElement();
        TaskTagDialog ttd=new TaskTagDialog( getShell(),tt );
        if (ttd.open()==Window.OK){
          tags.remove( tt ); // name may have changed
          tags.add( ttd.getTaskTag() );
          update();
        }
      }
    } );

    final Button bRemove=new Button(cButtons,SWT.PUSH);
    bRemove.setText( UITexts.generic_remove );
    bRemove.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_FILL) );
    bRemove.setEnabled( false );
    bRemove.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected(final org.eclipse.swt.events.SelectionEvent e) {
        TaskTag tt=(TaskTag)((IStructuredSelection)tableViewer.getSelection()).getFirstElement();
        tags.remove( tt );
        update();
      }
    } );

    tableViewer.addSelectionChangedListener( new ISelectionChangedListener() {

      @Override
      public void selectionChanged( final SelectionChangedEvent paramSelectionChangedEvent ) {
        boolean sel=((IStructuredSelection)tableViewer.getSelection()).size()==1;
        bRemove.setEnabled( sel );
        bEdit.setEnabled( sel );
      }
    } );


    DialogField df=createBooleanField( parent, UITexts.tasks_pref_case, EDITOR_TASK_TAGS_CASE );
    GridData gdCase=new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL);
    gdCase.horizontalSpan=2;
    df.setLayoutData( gdCase );

    tags.clear();
    tags.addAll(TaskTag.getTasksTags( getPreferenceStore() ));
    setInput();

    return control;
  }

  private void setInput(){
    List<TaskTag> lTags=new ArrayList<TaskTag>(tags);
    Collections.sort( lTags,new Comparator<TaskTag>() {
      /* (non-Javadoc)
       * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
       */
      @Override
      public int compare( final TaskTag o1, final TaskTag o2 ) {
        int ix=tTags.getSortColumn().getText().equals( UITexts.tasks_pref_tag )?0:1;
        String s1=ix==0?o1.getName():o1.getPriority();
        String s2=ix==0?o2.getName():o2.getPriority();
        return tTags.getSortDirection()==SWT.UP?s1.compareToIgnoreCase( s2 ):s2.compareToIgnoreCase( s1 );
      }
    } );
    tableViewer.setInput( lTags );
  }

  private void update(){

    try {
      JSONObject obj=new JSONObject();
      JSONArray high=new JSONArray();
      obj.put( EDITOR_TASK_TAGS_HIGH, high );

      JSONArray normal=new JSONArray();
      obj.put( EDITOR_TASK_TAGS_NORMAL, normal );

      JSONArray low=new JSONArray();
      obj.put( EDITOR_TASK_TAGS_LOW, low );

      for (TaskTag tt:tags){
        JSONArray arr=tt.getPriority().equals( UITexts.tasks_pref_priority_high )?high
            :tt.getPriority().equals( UITexts.tasks_pref_priority_normal )?normal
                :low;
        arr.put( tt.getName() );
      }

      getPreferenceStore().setValue( EDITOR_TASK_TAGS, obj.toString() );
      setInput();

    } catch (JSONException je){
      HaskellUIPlugin.log( je );
    }
  }


  /**
   * the lavel provider for task tags
   * @author JP Moresmau
   *
   */
  private static class TaskTagLabelProvider extends LabelProvider implements ITableLabelProvider{

    @Override
    public Image getColumnImage( final Object paramObject, final int paramInt ) {
      return null;
    }

    @Override
    public String getColumnText( final Object paramObject, final int paramInt ) {
      TaskTag tt=(TaskTag)paramObject;
      if (0==paramInt){
        return tt.getName();
      }
      return tt.getPriority();
    }

  }

  /**
   * the add/edit dialog for a task tag
   * @author JP Moresmau
   *
   */
  private static class TaskTagDialog extends Dialog {
    private TaskTag tt;


    public TaskTagDialog( final Shell parentShell ) {
      super( parentShell );
    }

    public TaskTagDialog( final Shell parentShell,final TaskTag tt ) {
      super( parentShell );
      this.tt=tt.clone();
    }

    /* (non-Javadoc)
     * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
     */
    @Override
    protected void configureShell( final Shell newShell ) {
      super.configureShell( newShell );
      newShell.setText( tt!=null?UITexts.tasks_pref_edit:UITexts.tasks_pref_new );
    }

    /* (non-Javadoc)
     * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar( final Composite parent ) {
      super.createButtonsForButtonBar( parent );
      update();
    }

    private void update(){
      Button b=getButton( OK );
      if (b!=null && !b.isDisposed()){
        b.setEnabled( tt!=null && tt.getName()!=null && tt.getName().length()>0 && tt.getPriority()!=null && tt.getPriority().length()>0 );
      }
    }


    /**
     * @return the tt
     */
    public TaskTag getTaskTag() {
      return tt;
    }

    /* (non-Javadoc)
     * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea( final Composite parent ) {
      Composite control= (Composite)super.createDialogArea( parent );

      GridLayout layout = new GridLayout(2,false);
      control.setLayout( layout );

      Label lTag=new Label(control,SWT.NONE);
      lTag.setText( UITexts.tasks_pref_tag );

      final Text tTag=new Text(control, SWT.BORDER);
      tTag.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL) );
      if (tt!=null){
        tTag.setText( tt.getName() );
      }

      Label lPriority=new Label(control,SWT.NONE);
      lPriority.setText( UITexts.tasks_pref_priority );

      final Combo cPriority=new Combo(control,SWT.READ_ONLY);
      cPriority.setItems( new String[]{UITexts.tasks_pref_priority_high,UITexts.tasks_pref_priority_normal,UITexts.tasks_pref_priority_low} );
      cPriority.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL) );

      if (tt!=null){
        for (int a=0;a<cPriority.getItemCount();a++){
          if (cPriority.getItem( a ).equals(tt.getPriority())){
            cPriority.select( a );
            break;
          }
        }
      } else {
        cPriority.select( 1 );
        tt=new TaskTag( tTag.getText(), cPriority.getItem( 1 ) );
      }

      tTag.addModifyListener( new ModifyListener() {

        @Override
        public void modifyText( final ModifyEvent arg0 ) {
         if (tt==null){
           tt=new TaskTag( tTag.getText(), cPriority.getItem( cPriority.getSelectionIndex() ) );
         } else {
           tt.setName(tTag.getText());
         }
         update();
        }
      } );

      cPriority.addSelectionListener( new SelectionAdapter() {
        /* (non-Javadoc)
         * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
         */
        @Override
        public void widgetSelected( final SelectionEvent e ) {
          if (tt==null){
            tt=new TaskTag( tTag.getText(), cPriority.getItem( cPriority.getSelectionIndex() ) );
          } else {
            tt.setPriority(cPriority.getItem( cPriority.getSelectionIndex() ));
          }
          update();
        }
      } );
      return control;
    }
  }
}
