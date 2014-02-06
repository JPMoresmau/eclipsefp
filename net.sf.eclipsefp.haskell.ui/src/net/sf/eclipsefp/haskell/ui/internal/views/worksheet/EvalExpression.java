/**
 *  Copyright (c) 2014 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.views.worksheet;

import net.sf.eclipsefp.haskell.buildwrapper.types.EvalResult;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;


/**
 * An evaluation expression
 * @author JP Moresmau
 *
 */
public class EvalExpression implements Comparable<EvalExpression>{
  /**
   * the expression to evaluate
   */
  private String expression;
  /**
   * the last result of the evaluation
   */
  private EvalResult lastResult;
  /**
   * the index in the list of evaluation for that file
   */
  private int index;

  private ResultType resultType=ResultType.TEXT;

  /**
   *
   */
  public EvalExpression() {

  }

  public EvalExpression(final IMarker m){
    expression=m.getAttribute( WorkSheetViewPage.MARKER_EXPRESSION, "" );
    index=m.getAttribute( WorkSheetViewPage.MARKER_INDEX, 0 );
    try {
      resultType=ResultType.valueOf( m.getAttribute( WorkSheetViewPage.MARKER_RESULT_TYPE, ResultType.TEXT.toString() ) );
    } catch (IllegalArgumentException ignore){
      // noop
    }
   }

  /**
   * save the expression as a marker on the file
   * @param f the file
   * @return the generated marker
   * @throws CoreException
   */
  public IMarker addMarker(final IFile f) throws CoreException{
    IMarker m=f.createMarker( WorkSheetViewPage.MARKER_TYPE );
    m.setAttribute( WorkSheetViewPage.MARKER_EXPRESSION, expression );
    m.setAttribute( WorkSheetViewPage.MARKER_INDEX, index );
    m.setAttribute( WorkSheetViewPage.MARKER_RESULT_TYPE, String.valueOf(resultType ));
    return m;
  }

  /**
   * do we have an expression to evaluate
   * @return
   */
  public boolean isValid(){
    return expression!=null && expression.trim().length()>0;
  }

  /* (non-Javadoc)
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo( final EvalExpression o ) {
    return Integer.valueOf(index).compareTo( o.index);
  }

  public String getExpression() {
    return expression;
  }


  public void setExpression( final String expression ) {
    this.expression = expression;
  }



  public EvalResult getLastResult() {
    return lastResult;
  }



  public void setLastResult( final EvalResult lastResult ) {
    this.lastResult = lastResult;
  }


  public int getIndex() {
    return index;
  }


  public void setIndex( final int index ) {
    this.index = index;
  }


  public ResultType getResultType() {
    return resultType;
  }


  public void setResultType( final ResultType resultType ) {
    this.resultType = resultType;
  }


}
