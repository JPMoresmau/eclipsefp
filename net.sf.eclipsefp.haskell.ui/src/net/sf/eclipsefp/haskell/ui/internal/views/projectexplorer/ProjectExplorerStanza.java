package net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer;

import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import org.eclipse.core.resources.IFile;

/**
 * Cabal stanza in Project explorer
 * @author JP Moresmau
 *
 */
public class ProjectExplorerStanza {
  private IFile owner;
  private PackageDescriptionStanza stanza;

  public ProjectExplorerStanza( final IFile owner, final PackageDescriptionStanza stanza ) {
    super();
    this.owner = owner;
    this.stanza = stanza;
  }

  public IFile getOwner() {
    return owner;
  }

  public void setOwner( final IFile owner ) {
    this.owner = owner;
  }

  public PackageDescriptionStanza getStanza() {
    return stanza;
  }

  public void setStanza( final PackageDescriptionStanza stanza ) {
    this.stanza = stanza;
  }


}
