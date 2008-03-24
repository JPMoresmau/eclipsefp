// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.project;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

/** <p>implementors can be added to a ProjectCreationOperation and will
  * be executed after the project has been created.</p>
  *
  * @author Leif Frenzel
  */
public interface IProjectCreationOperationExtraOp {

  void run( final IProject project,
            final IProgressMonitor monitor ) throws CoreException;
}
