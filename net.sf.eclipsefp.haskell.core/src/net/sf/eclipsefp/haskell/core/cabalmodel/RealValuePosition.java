package net.sf.eclipsefp.haskell.core.cabalmodel;


public class RealValuePosition extends ValuePosition {
  private final String realValue;

  public RealValuePosition(final ValuePosition pos,final String realValue) {
    super( pos.getStartLine(), pos.getEndLine(), pos.getInitialIndent() );
    this.realValue=realValue;
 }

  public String getRealValue() {
    return realValue;
  }

  @Override
  public String toString() {
   return realValue+" "+ super.toString(); //$NON-NLS-1$
  }

}
