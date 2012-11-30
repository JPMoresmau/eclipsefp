/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.core.cabalmodel;

/**
 * Position of a value in a cabal file
 * @author JP Moresmau
 *
 */
public class ValuePosition implements Cloneable{
    private int startLine=-1;
    private int endLine=-1;
    private int initialIndent=-1;
    private int subsequentIndent=-1;


    public ValuePosition() {
      super();
    }

    public ValuePosition( final int startLine, final int endLine, final int indent ) {
      super();
      this.startLine = startLine;
      this.endLine = endLine;
      this.initialIndent = indent;
    }

    @Override
    protected ValuePosition clone()  {
      ValuePosition vp= new ValuePosition( startLine, endLine, initialIndent );
      vp.subsequentIndent=subsequentIndent;
      return vp;
    }

    public int getStartLine() {
      return startLine;
    }

    public int getEndLine() {
      return endLine;
    }

    public int getInitialIndent() {
      return initialIndent;
    }


    public void setStartLine( final int startLine ) {
      this.startLine = startLine;
    }


    public void setEndLine( final int endLine ) {
      this.endLine = endLine;
    }


    public void setInitialIndent( final int indent ) {
      this.initialIndent = indent;
    }

    public void diffLine ( final int diff ) {
      this.startLine += diff;
      this.endLine += diff;
    }

    public int getSubsequentIndent() {
      return subsequentIndent;
    }


    public void setSubsequentIndent( final int subsequentIndent ) {
      this.subsequentIndent = subsequentIndent;
    }

    @Override
    public String toString() {
      return getStartLine()+"->"+getEndLine()+"("+getInitialIndent()+","+getSubsequentIndent()+")";    //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$//$NON-NLS-4$
    }
}
