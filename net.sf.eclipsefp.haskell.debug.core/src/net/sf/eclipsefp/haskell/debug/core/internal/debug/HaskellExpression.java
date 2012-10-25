/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.core.internal.debug;

import org.eclipse.debug.core.model.IExpression;
import org.eclipse.debug.core.model.IValue;


/**
 * Wrapping a value and an expression text
 * @author JP Moresmau
 *
 */
public class HaskellExpression extends HaskellDebugElement implements IExpression{
  private final String expression;
  private final HaskellValue value;

  public HaskellExpression( final HaskellDebugTarget target,final String expression,final HaskellValue value ) {
    super( target );
    this.expression=expression;
    this.value=value;
  }

  @Override
  public String getExpressionText() {
    return expression;
  }

  @Override
  public IValue getValue() {
    return value;
  }

  @Override
  public void dispose() {
      // NOOP

  }



}
