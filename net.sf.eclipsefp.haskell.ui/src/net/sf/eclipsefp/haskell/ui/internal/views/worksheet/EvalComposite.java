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
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;


/**
 * This composite displays one evaluation expression and it result or error
 * Double clicking allow edition of the expression
 * There is also a button to delete the expression
 * @author JP Moresmau
 *
 */
public class EvalComposite extends Composite implements EvalHandler{
  private final EvalExpression expression;
  private Control lResult;
  private final Label lResultIcon;
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

    MouseListener dbl=new MouseAdapter() {

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
    lExpr.addMouseListener( dbl);
    lResult.addMouseListener( dbl );
    lResultIcon.addMouseListener( dbl );
    this.addMouseListener( dbl );

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
      default:
        buildText( re );
    }

    lResultIcon.setImage(HaskellUIImages.getImage( IImageNames.WORKSHEET_OK ));
  }

  private void setCurrentControl(final Control c){
    lResult=c;
    GridData gdResult=new GridData(GridData.FILL_HORIZONTAL);
    gdResult.horizontalSpan=2;
    lResult.setLayoutData( gdResult );
    lResult.setBackground( getBackground() );
  }

  private void buildHTML(final String html){
    Browser b=null;
    if (!(lResult instanceof Browser)){
      if (lResult!=null){
        lResult.dispose();
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
    } else{
      b=(Browser)lResult;
    }
    setCurrentControl(b);
    b.setText(LangUtil.unquote(html) );

  }


  private void buildText(final String txt){
    Text t=null;
    if (!(lResult instanceof Text)){
      if (lResult!=null){
        lResult.dispose();
        lResult=null;
      }
      t=new Text(this,SWT.MULTI | SWT.WRAP);
      t.setEditable( false );
    } else {
      t=(Text)lResult;
    }
    t.setText( txt );
    setCurrentControl(t);
  }

  private void buildErrorControl(final String error){
    buildText(error);
    lResultIcon.setImage(PlatformUI.getWorkbench().getSharedImages().getImage( ISharedImages.IMG_OBJS_ERROR_TSK ) );

  }

  private void buildEmptyControl(){
    lResultIcon.setImage( null );
    buildText( "" );
  }


}
