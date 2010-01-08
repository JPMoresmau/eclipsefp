package net.sf.eclipsefp.haskell.debug.core.internal.debug;

import net.sf.eclipsefp.haskell.core.util.GHCiSyntax;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.core.model.IVariable;

/**
 * value of an haskell binding
 * @author JP Moresmau
 *
 */
public class HaskellValue extends HaskellDebugElement implements IValue {
  private final String val;
  private final HaskellVariable var;

  public HaskellValue(final HaskellVariable var,final String val){
    super( var.getDebugTarget() );
    this.var=var;
    this.val=val;

  }

  public String getReferenceTypeName() {
   return var.getReferenceTypeName();
  }

  public String getValueString()  {
   return val;
  }

  public IVariable[] getVariables() {
    return new IVariable[0];
  }

  public boolean hasVariables() {
    return false;
  }

  public boolean isAllocated()  {
   return !GHCiSyntax.UNRESOLVED.equals(val);
  }


}
