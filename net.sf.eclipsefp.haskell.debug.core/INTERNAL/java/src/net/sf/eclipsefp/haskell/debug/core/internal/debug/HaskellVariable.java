package net.sf.eclipsefp.haskell.debug.core.internal.debug;

import java.util.regex.Matcher;
import net.sf.eclipsefp.haskell.core.util.GHCiSyntax;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.core.model.IVariable;

/**
 * haskell binding
 * @author jean-philippem
 *
 */
public class HaskellVariable implements IVariable {
  private final String name;
  private String type;
  private String value;
  private final HaskellStrackFrame frame;

  public HaskellVariable(final String line,final HaskellStrackFrame frame){
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
    Matcher m=GHCiSyntax.BINDING_PATTERN.matcher( line );
    if (m.matches()){
      name=m.group( 1 );
      type=m.group( 2 );
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

  public String getName() {
   return name;
  }

  public String getReferenceTypeName()  {
   return type;
  }

  public IValue getValue()  {
   return new HaskellValue( this, value );
  }

  public boolean hasValueChanged()  {
    return false;
  }

  public IDebugTarget getDebugTarget() {
   return frame.getDebugTarget();
  }

  public ILaunch getLaunch() {
    return frame.getLaunch();
  }

  public String getModelIdentifier() {
    return frame.getModelIdentifier();
  }

  public Object getAdapter( final Class adapter ) {
    if (adapter.isAssignableFrom(this.getClass() )){
      return this;
    }
    return null;
  }

  public void setValue( final String expression )  {
    //NOOP
  }

  public void setValue( final IValue value )  {
    //NOOP
  }

  public boolean supportsValueModification() {
    return false;
  }

  public boolean verifyValue( final String expression )  {
    return false;
  }

  public boolean verifyValue( final IValue val )  {
    return false;
  }

}
