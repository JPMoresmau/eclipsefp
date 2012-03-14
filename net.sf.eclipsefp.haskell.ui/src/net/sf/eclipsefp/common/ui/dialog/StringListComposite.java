/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.common.ui.dialog;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;


/**
 * A simple composite that lets the user add and remove a list of strings
 * @author JP Moresmau
 *
 */
public class StringListComposite  extends Composite {
  protected List<String> paths=new ArrayList<String>();

  protected ListViewer viewer;

  /**
   * the custom title for the add dialog box
   */
  private String addTitle;
  /**
   * the custom message for the add dialog box
   */
  private String addMessage;

  /**
   * @param parent
   * @param style
   */
  public StringListComposite( final Composite parent, final int style ) {
    super( parent, style );
    initUI();
  }

  private void initUI(){
    setLayout(new GridLayout(2,false));
    viewer=new ListViewer( this, SWT.H_SCROLL | SWT.V_SCROLL | SWT.MULTI | SWT.BORDER);
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new LabelProvider() );
    GridData gdViewer=new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL);
    gdViewer.verticalSpan=2;
    viewer.getList().setLayoutData(gdViewer );

    Button bAdd=new Button(this,SWT.PUSH);
    bAdd.setLayoutData( new GridData(GridData.VERTICAL_ALIGN_BEGINNING | GridData.HORIZONTAL_ALIGN_FILL) );
    bAdd.setText( UITexts.cabalEditor_add );

    bAdd.addSelectionListener(  new SelectionAdapter() {
      @Override
      public void widgetSelected(final org.eclipse.swt.events.SelectionEvent e) {
        String s=onAdd();
        if (s!=null){
          paths.add( s );
          viewer.refresh();
        }
      }
    } );



    Button bRemove=new Button(this,SWT.PUSH);
    bRemove.setLayoutData( new GridData(GridData.VERTICAL_ALIGN_BEGINNING | GridData.HORIZONTAL_ALIGN_FILL) );
    bRemove.setText( UITexts.cabalEditor_remove );

    bRemove.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected(final org.eclipse.swt.events.SelectionEvent e) {
        for (int idx:viewer.getList().getSelectionIndices()){
          paths.remove( idx );
        }
       viewer.refresh();
      }
    } );

  }

  protected String onAdd(){
    InputDialog id=new InputDialog(getShell(), getAddTitle(), getAddMessage(),"",null);
    if (id.open()==Window.OK){
      return id.getValue();
    }
    return null;
  }

  /**
   * @return the paths
   */
  public List<String> getPaths() {
    return Collections.unmodifiableList(paths);
  }


  /**
   * @param paths the paths to set
   */
  public void setPaths( final List<String> paths ) {
    this.paths =new ArrayList<String>(paths);
    if (viewer!=null){
      viewer.setInput( this.paths );
    }
  }


  public String getAddTitle() {
    return addTitle;
  }


  public void setAddTitle( final String addTitle ) {
    this.addTitle = addTitle;
  }


  public String getAddMessage() {
    return addMessage;
  }


  public void setAddMessage( final String addMessage ) {
    this.addMessage = addMessage;
  }
}
