/**
 *  Copyright (c) 2014 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.views.worksheet;

import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.model.WorkbenchViewerComparator;


/**
 * Input dialog for expressions and their rendering type
 * @author JP Moresmau
 *
 */
public class EvalExpressionDialog extends InputDialog {
  private ResultType resultType=ResultType.TEXT;

  /**
   * @param parentShell
   * @param dialogTitle
   * @param dialogMessage
   * @param initialValue
   */
  public EvalExpressionDialog( final Shell parentShell, final String dialogTitle,
      final String dialogMessage, final EvalExpression initialValue) {
    super( parentShell, dialogTitle, dialogMessage, initialValue.getExpression(), new EvalExpressionValidator() );
    this.resultType=initialValue.getResultType();
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.dialogs.InputDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent ) {
    Composite c=(Composite)super.createDialogArea( parent );

    Composite typeC=new Composite(c,SWT.NONE);
    typeC.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );
    typeC.setLayout( new GridLayout(2,false) );
    Label typeL=new Label(typeC,SWT.NONE);
    typeL.setText( UITexts.worksheet_expression_type );

    ComboViewer cv=new ComboViewer( typeC,SWT.READ_ONLY | SWT.SINGLE);
    cv.setContentProvider( new ArrayContentProvider() );
    cv.setLabelProvider( new LabelProvider() );
    cv.setComparator( new WorkbenchViewerComparator() );

    cv.setInput( ResultType.values() );
    cv.setSelection( new StructuredSelection(resultType) );
    cv.addSelectionChangedListener( new ISelectionChangedListener() {

      @Override
      public void selectionChanged( final SelectionChangedEvent event ) {
       resultType=(ResultType)((IStructuredSelection)event.getSelection()).getFirstElement();
      }
    } );
    return c;
  }


  /**
   * @return the resultType
   */
  public ResultType getResultType() {
    return resultType;
  }

  private static class EvalExpressionValidator implements IInputValidator {
    /* (non-Javadoc)
     * @see org.eclipse.jface.dialogs.IInputValidator#isValid(java.lang.String)
     */
    @Override
    public String isValid( final String newText ) {
      if (newText==null || newText.trim().length()==0){
        return UITexts.worksheet_expression_empty;
      }
      return null;
    }
  }
}
