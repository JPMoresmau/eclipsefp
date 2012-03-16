package net.sf.eclipsefp.haskell.debug.core.internal.debug;

import java.util.regex.Matcher;
import net.sf.eclipsefp.haskell.core.util.GHCiSyntax;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.core.model.IVariable;

/**
 * haskell binding
 * @author JP Moresmau
 *
 */
public class HaskellVariable extends HaskellDebugElement implements IVariable {
  private String name;
  private String type;
  private String value;
  private final HaskellStrackFrame frame;

  public HaskellVariable(final String line,final HaskellStrackFrame frame){
    super( frame.getDebugTarget() );
    this.frame=frame;
    /*
    int ix1=line.indexOf( "::" );
    int ix2=line.indexOf( "=",ix1+2 );
    name=line.substring(0,ix1).trim();
    if (ix2>-1){
      type=line.substring(ix1+2,ix2).trim();
      value=line.substring(ix2+1).trim();
    } else {
      type=line.substring(ix1+2).trim();
      value=GHCiSyntax.UNRESOLVED;
    }*/
    processLine(line);

  }


  public HaskellStrackFrame getFrame() {
    return frame;
  }

  private void processLine(final String line){
    Matcher m=GHCiSyntax.BINDING_PATTERN.matcher( line );
    if (m.matches()){
      name=m.group( 1 );
      type=GHCiSyntax.formatType( m.group( 2 ));
      value=m.group( 4 );
      if (value==null){
        value=GHCiSyntax.UNRESOLVED;
      }
    } else {
      name=line;
      type=GHCiSyntax.UNRESOLVED;
      value=GHCiSyntax.UNRESOLVED;
    }
  }

  @Override
  public String getName() {
   return name;
  }

  @Override
  public String getReferenceTypeName()  {
   return type;
  }

  @Override
  public IValue getValue()  {
   return new HaskellValue( this, value );
  }

  @Override
  public boolean hasValueChanged()  {
    return false;
  }


  @Override
  public void setValue( final String expression ) throws DebugException {
    // for the time being, setting the expression is only a way of forcing the evaluation
    frame.getDebugTarget().forceVariable( this );
    /* if (line!=null){
       processLine( line );
     }*/
  }

  @Override
  public void setValue( final IValue value ) throws DebugException {
    setValue(value!=null?value.getValueString():""); //$NON-NLS-1$
  }

  @Override
  public boolean supportsValueModification() {
    return GHCiSyntax.UNRESOLVED.equals( value );
  }

  @Override
  public boolean verifyValue( final String expression )  {
    return GHCiSyntax.UNRESOLVED.equals( value );
  }

  @Override
  public boolean verifyValue( final IValue val )  {
    return GHCiSyntax.UNRESOLVED.equals( value );
  }

}
