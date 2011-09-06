package net.sf.eclipsefp.haskell.core.cabalmodel;


public class ValuePosition {
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
