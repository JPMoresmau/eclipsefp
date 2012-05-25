/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.core.project;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;

/**
 *
 * @author JP Moresmau
 *
 */
public class YesodNature implements IProjectNature {
  public static final String NATURE_ID = YesodNature.class.getName();
  private IProject project;

  @Override
  public void configure()  {
    // NOOP, marker nature

  }

  @Override
  public void deconfigure()  {
    // NOOP

  }

  @Override
  public IProject getProject() {
   return project;
  }

  @Override
  public void setProject( final IProject paramIProject ) {
   this.project=paramIProject;

  }

}
