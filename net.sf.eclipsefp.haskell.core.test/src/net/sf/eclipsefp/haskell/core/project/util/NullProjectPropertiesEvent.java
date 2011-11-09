package net.sf.eclipsefp.haskell.core.project.util;

import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.project.IProjectPropertiesEvent;

@Deprecated
public class NullProjectPropertiesEvent implements IProjectPropertiesEvent {

	public Object getNewValue() { return null; }

	public Object getOldValue() { return null; }

	public String getPropertyName() { return null; }

	public IHaskellProject getSource() { return null; }

}
