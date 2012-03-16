package net.sf.eclipsefp.haskell.debug.ui.internal.debug;

import net.sf.eclipsefp.haskell.debug.core.internal.debug.HaskellDebugElement;
import net.sf.eclipsefp.haskell.debug.core.internal.debug.HaskellValue;
import net.sf.eclipsefp.haskell.debug.ui.internal.util.UITexts;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IDebugElement;
import org.eclipse.debug.core.model.ISuspendResume;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.core.model.IWatchExpressionDelegate;
import org.eclipse.debug.core.model.IWatchExpressionListener;
import org.eclipse.debug.core.model.IWatchExpressionResult;

/**
 * Delegate to evaluate haskell expression in GHCi debugging session
 * @author JP Moresmau
 *
 */
public class HaskellWatchExpressionDelegate implements IWatchExpressionDelegate {

  @Override
  public void evaluateExpression( final String expression, final IDebugElement context,
      final IWatchExpressionListener listener ) {
      HaskellDebugElement hde=(HaskellDebugElement)context;
      if ((hde instanceof ISuspendResume &&  ((ISuspendResume)hde).isSuspended()) || hde.getDebugTarget().isSuspended()){
        try {
          HaskellValue val=hde.getDebugTarget().evaluate( expression );
          listener.watchEvaluationFinished( new WatchExpressionResult( expression, val, null ));

        } catch (DebugException de){
          listener.watchEvaluationFinished( new WatchExpressionResult( expression, null, de ));

        }
      } else {
        listener.watchEvaluationFinished( new WatchExpressionResult(expression,null,null) );
      }

  }

  /**
   * Wraps either:
   * - a HaskellValue if we could evaluate the expression
   * - A DebugException if there was an error
   * - Nothing if we couldn't evaluate at all because we weren't suspended
   * @author jean-philippem
   *
   */
  private class WatchExpressionResult implements IWatchExpressionResult{
    private final HaskellValue val;
    private final DebugException exception;
    private final String expression;



    public WatchExpressionResult( final String expression, final HaskellValue val,
        final DebugException exception ) {
      this.expression = expression;
      this.val = val;
      this.exception = exception;
    }

    @Override
    public boolean hasErrors() {
      return exception!=null;
    }

    @Override
    public IValue getValue() {
      return val;
    }

    @Override
    public String getExpressionText() {
      return expression;
    }

    @Override
    public DebugException getException() {
      return exception;
    }

    @Override
    public String[] getErrorMessages() {
      if (val==null && exception==null){
        return new String[]{UITexts.evaluate_need_suspend};
      }
      return new String[0];
    }
  }

}
