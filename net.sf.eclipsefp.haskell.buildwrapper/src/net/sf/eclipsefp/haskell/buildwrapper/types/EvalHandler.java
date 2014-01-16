/**
 *  Copyright (c) 2014 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.types;

/**
 * an EvalHadler provides the expression to evaluate and the handling of the result
 * @author JP Moresmau
 *
 */
public interface EvalHandler {
	String getExpression();
	void handleResult(EvalResult er);
}
