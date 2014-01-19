/**
 *  Copyright (c) 2014 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.views.worksheet;

import net.sf.eclipsefp.haskell.buildwrapper.types.EvalHandler;
import net.sf.eclipsefp.haskell.buildwrapper.types.EvalResult;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import net.sf.eclipsefp.haskell.util.LangUtil;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.ProgressEvent;
import org.eclipse.swt.browser.ProgressListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;


/**
 * This composite displays one evaluation expression and it result or error
 * Double clicking allow edition of the expression
 * There is also a button to delete the expression
 * @author JP Moresmau
 *
 */
public class EvalComposite extends Composite implements EvalHandler{
  private final EvalExpression expression;
  private IControlProvider lResult;
  private final Label lResultIcon;
  private final MouseListener dblListener;

  /**
   * @param arg0
   * @param arg1
   */
  public EvalComposite(final WorkSheetViewPage page, final EvalExpression expression) {
    super( page.getMainComposite(), SWT.NONE );
    this.expression=expression;
    GridLayout gl=new GridLayout(3,false);
    this.setLayout( gl );
    final Text lExpr=new Text(this,SWT.NONE);
    lExpr.setEditable( false );
    lExpr.setBackground( getBackground() );
    /*Font f=lExpr.getFont();
    f.getFontData()[0].setStyle( f.getFontData()[0].getStyle()| SWT.ITALIC );
    f.getFontData()
    lExpr.setFont( f );*/
    lExpr.setFont(JFaceResources.getFontRegistry().getItalic(lExpr.getFont().getFontData()[0].getName()));
    GridData gdExpr=new GridData(GridData.FILL_HORIZONTAL) ;
    gdExpr.horizontalSpan=2;
    lExpr.setLayoutData(gdExpr);
    lExpr.setText( getExpression() );
    //Button bRemove=new Button(this,SWT.PUSH | SWT.FLAT);
    //bRemove.setImage( PlatformUI.getWorkbench().getSharedImages().getImage( ISharedImages.IMG_ELCL_REMOVE ) );

    ToolBar tb=new ToolBar( this, SWT.FLAT );
    ToolItem tiRemove=new ToolItem( tb, SWT.PUSH );
    tiRemove.setImage( PlatformUI.getWorkbench().getSharedImages().getImage( ISharedImages.IMG_ELCL_REMOVE ) );
    tiRemove.setToolTipText( UITexts.worksheet_removeexpression_tooltip );

    lResultIcon=new Label( this, SWT.NONE );
    lResultIcon.setBackground( getBackground() );
    lResultIcon.setLayoutData( new GridData(GridData.VERTICAL_ALIGN_BEGINNING) );

    dblListener=new MouseAdapter() {

      @Override
      public void mouseDoubleClick( final MouseEvent event ) {
        EvalExpressionDialog id=new EvalExpressionDialog( getShell(), UITexts.worksheet_editexpression_title, UITexts.worksheet_addexpression_message, expression );
          if (id.open()==Window.OK){
            expression.setExpression( id.getValue() );
            expression.setResultType( id.getResultType() );
            lExpr.setText( getExpression() );
            page.save();
            page.eval();
          }

      }
    };


    buildEmptyControl();


    tiRemove.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected(final org.eclipse.swt.events.SelectionEvent arg0) {
        String msg=NLS.bind( UITexts.worksheet_removeexpression_message,expression.getExpression());
        if (MessageDialog.openConfirm( getShell(), UITexts.worksheet_removeexpression_title, msg )){
          page.remove( EvalComposite.this );
        }
      }
    } );


    lExpr.addMouseListener( dblListener);
    lResultIcon.addMouseListener( dblListener );
    this.addMouseListener( dblListener );

    setBackground( getDisplay().getSystemColor( SWT.COLOR_LIST_BACKGROUND ) );
  }

  public EvalExpression getEvalExpression() {
    return expression;
  }

  /* (non-Javadoc)
   * @see org.eclipse.swt.widgets.Control#setBackground(org.eclipse.swt.graphics.Color)
   */
  @Override
  public void setBackground( final Color arg0 ) {
    super.setBackground( arg0 );
    for (Control c:getChildren()){
      c.setBackground( arg0 );
    }
  }

  /**
   * @return the expression
   */
  @Override
  public String getExpression() {
    return expression.getExpression();
  }

  /* (non-Javadoc)
   * @see org.eclipse.swt.widgets.Control#setToolTipText(java.lang.String)
   */
  @Override
  public void setToolTipText( final String arg0 ) {
    super.setToolTipText( arg0 );
    for (Control c:getChildren()){
      if (!(c instanceof ToolBar)){
        c.setToolTipText( arg0 );
      }
    }
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.buildwrapper.types.EvalHandler#handleResult(net.sf.eclipsefp.haskell.buildwrapper.types.EvalResult)
   */
  @Override
  public void handleResult( final EvalResult er ) {
    expression.setLastResult( er );
    if (this.isDisposed()){
      return;
    }
    getDisplay().syncExec( new Runnable(){
      /* (non-Javadoc)
       * @see java.lang.Runnable#run()
       */
      @Override
      public void run() {
        setToolTipText("");

        buildControl( er );
        if (er.getType()!=null && er.getType().length()>0){
          setToolTipText( er.getType() );
        }
        layout(true);
        getParent().layout(true);

      }
    } );

  }

  private void buildControl(final EvalResult er){
    if (er.getResult()!=null && er.getResult().length()>0){
      buildResultControl(er.getResult());
    } else if (er.getError()!=null && er.getError().length()>0){
      buildErrorControl( er.getError()  );
    } else {
      buildEmptyControl();
    }
  }

  private void buildResultControl(final String re){
    switch (expression.getResultType()){
      case HTML:
        buildHTML( re );
        break;
      case JSON:
        buildJSON( re );
        break;
      default:
        buildText( re );
    }

    lResultIcon.setImage(HaskellUIImages.getImage( IImageNames.WORKSHEET_OK ));
  }

  private void setCurrentControl(final IControlProvider c){
    lResult=c;
    GridData gdResult=new GridData(GridData.FILL_HORIZONTAL);
    gdResult.horizontalSpan=2;
    lResult.getControl().setLayoutData( gdResult );
    lResult.getControl().setBackground( getBackground() );
  }

  private void buildHTML(final String html){
    IControlProvider cp=null;
    Browser b=null;
    if (lResult==null || !(lResult.getControl() instanceof Browser)){
      if (lResult!=null){
        lResult.getControl().dispose();
        lResult=null;
      }
      final Browser fb=new Browser(this,SWT.TOP | SWT.RESIZE);
      b=fb;
      b.addProgressListener(new ProgressListener() {
        @Override
        public void completed(final ProgressEvent event) {
          int contentHeight = ((Double)fb.evaluate("return document.body.scrollHeight")).intValue();
          int contentWidth = ((Double)fb.evaluate("return document.body.scrollWidth")).intValue();
          GridData gdResult=new GridData(GridData.FILL_HORIZONTAL);
          gdResult.horizontalSpan=2;
          gdResult.heightHint=contentHeight;
          gdResult.widthHint=contentWidth;
          fb.setLayoutData( gdResult );

          EvalComposite.this.layout(true);
          EvalComposite.this.getParent().layout(true);
        }
        @Override
        public void changed(final ProgressEvent arg0) {
          EvalComposite.this.layout(true);
          EvalComposite.this.getParent().layout(true);
        }
      });
      b.addMouseListener( dblListener );
      cp=new ControlProvider( b );
    } else{
      cp=lResult;
      b=(Browser)lResult.getControl();
    }
    setCurrentControl(cp);
    b.setText(LangUtil.unquote(html) );

  }


  private void buildText(final String txt){
    IControlProvider cp=null;
    Text t=null;
    if (lResult==null || !(lResult.getControl() instanceof Text)){
      if (lResult!=null){
        lResult.getControl().dispose();
        lResult=null;
      }
      t=new Text(this,SWT.MULTI | SWT.WRAP);
      t.setEditable( false );
      t.addMouseListener( dblListener );
      cp=new ControlProvider(t);
    } else {
      cp=lResult;
      t=(Text)lResult.getControl();
    }
    t.setText( txt );
    setCurrentControl(cp);
  }

  private void buildErrorControl(final String error){
    buildText(error);
    lResultIcon.setImage(PlatformUI.getWorkbench().getSharedImages().getImage( ISharedImages.IMG_OBJS_ERROR_TSK ) );

  }

  private void buildEmptyControl(){
    lResultIcon.setImage( null );
    buildText( "" );
  }

  private void buildJSON(final String json){
    Object root=null;
    String jsonUQ=LangUtil.unquote( json );
    try {
      root=new JSONObject(jsonUQ);
    } catch (JSONException je){
      try {
        root=new JSONArray( jsonUQ );
      } catch (JSONException je2){
        root=JSONObject.stringToValue( jsonUQ );
      }
    }
    if (root==null || root instanceof String){
      buildText(json);
    } else {
      TreeControlProvider tcp;
      if (lResult==null || !(lResult instanceof TreeControlProvider)){
        if (lResult!=null){
          lResult.getControl().dispose();
          lResult=null;
        }
       TreeViewer tv=new TreeViewer(this,SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL);
       tcp=new TreeControlProvider( tv );
       tcp.viewer.setContentProvider( new JSONContentProvider() );
       tcp.viewer.setComparator( new JSONContentProvider.JSONComparator() );
       Listener l=new Listener(){
         /* (non-Javadoc)
         * @see org.eclipse.swt.widgets.Listener#handleEvent(org.eclipse.swt.widgets.Event)
         */
        @Override
        public void handleEvent( final Event arg0 ) {
          // let the tree expand, then layout
          getDisplay().asyncExec( new Runnable() {

            @Override
            public void run() {
              EvalComposite.this.layout(true);
              EvalComposite.this.getParent().layout(true);

            }
          } );


        }
       };
       tcp.viewer.getTree().addListener( SWT.Expand, l );
       tcp.viewer.getTree().addListener( SWT.Collapse, l );
       tcp.getControl().addMouseListener( dblListener );
      } else {
        tcp=(TreeControlProvider)lResult;
      }
      tcp.viewer.setInput( root );
      setCurrentControl(tcp);
    }
  }

  /**
   * Simple interface to use as wrapper around a control
   * @author JP Moresmau
   *
   */
  private interface IControlProvider{
    Control getControl();
  }

  /**
   * Simple wrapper
   * @author JP Moresmau
   *
   */
  private class ControlProvider implements IControlProvider {
    private final Control ctrl;

    public ControlProvider( final Control ctrl ) {
      super();
      this.ctrl = ctrl;
    }

    @Override
    public Control getControl(){
      return ctrl;
    }
  }

  /**
   * we need to do things on the tree viewer, so wrap the viewer instead
   *
   * @author JP Moresmau
   *
   */
  private class TreeControlProvider implements IControlProvider {
    private final TreeViewer viewer;

    public TreeControlProvider(final TreeViewer viewer){
      this.viewer=viewer;
    }

    /* (non-Javadoc)
     * @see net.sf.eclipsefp.haskell.ui.internal.views.worksheet.EvalComposite.IControlProvider#getControl()
     */
    @Override
    public Control getControl() {
      return viewer.getControl();
    }
  }
}
