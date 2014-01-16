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
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.window.Window;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
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
  private final Text lResult;
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
    lResult=new Text(this,SWT.MULTI | SWT.WRAP);
    lResult.setEditable( false );
    lResult.setBackground( getBackground() );
    lResult.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );
    GridData gdResult=new GridData(GridData.FILL_HORIZONTAL);
    gdResult.horizontalSpan=2;
    lResult.setLayoutData( gdResult );

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
         InputDialog id=new InputDialog( getShell(), UITexts.worksheet_editexpression_title, UITexts.worksheet_addexpression_message, getExpression(), null );
          if (id.open()==Window.OK){
            expression.setExpression( id.getValue() );
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
  }

  public EvalExpression getEvalExpression() {
    return expression;
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
    getDisplay().syncExec( new Runnable(){
      /* (non-Javadoc)
       * @see java.lang.Runnable#run()
       */
      @Override
      public void run() {
        setToolTipText("");

        if (er.getResult()!=null && er.getResult().length()>0){
          lResult.setText( er.getResult() );
          lResultIcon.setImage(HaskellUIImages.getImage( IImageNames.WORKSHEET_OK ));
        } else if (er.getError()!=null && er.getError().length()>0){
          lResult.setText( er.getError() );
          lResultIcon.setImage(PlatformUI.getWorkbench().getSharedImages().getImage( ISharedImages.IMG_OBJS_ERROR_TSK ) );
        } else {
          lResultIcon.setImage( null );
          lResult.setText( "" );
        }
        if (er.getType()!=null && er.getType().length()>0){
          setToolTipText( er.getType() );
        }
        layout(true);
        getParent().layout(true);

      }
    } );

  }

}
