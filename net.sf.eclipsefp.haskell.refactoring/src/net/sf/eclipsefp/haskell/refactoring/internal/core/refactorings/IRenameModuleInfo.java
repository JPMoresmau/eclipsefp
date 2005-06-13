// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.refactoring.internal.core.refactorings;

import de.leiffrenzel.fp.haskell.core.halamo.IModule;

/** <p>Encapsulates information that is needed for creating a rename 
  * refactoring.</p>
  * 
  * @author Leif Frenzel
  */
public interface IRenameModuleInfo {

  IModule getModule();
  int getOffset();
  String getOldName();
  
  void setNewName( String newName );
  String getNewName();
  
  void setUpdateReferences( boolean updateReferences );
  boolean isUpdateReferences();
}
