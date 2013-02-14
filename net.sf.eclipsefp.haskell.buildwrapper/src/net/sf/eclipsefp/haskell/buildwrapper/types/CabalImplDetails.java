/**
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.types;

import java.util.ArrayList;
import java.util.List;


/**
 * @author JP Moresmau
 *
 */
public class CabalImplDetails {
  public enum SandboxType {
    NONE,
    CABAL_DEV;
  }
  
  private String executable;
  private final List<String> options=new ArrayList<String>();
  private SandboxType type=SandboxType.NONE;

  public CabalImplDetails() {
    super();
  }


  public String getExecutable() {
    return executable;
  }


  public List<String> getOptions() {
    return options;
  }



  public void setExecutable( final String executable ) {
    this.executable = executable;
  }


public SandboxType getType() {
	return type;
}


public void setType(SandboxType type) {
	this.type = type;
}


}
