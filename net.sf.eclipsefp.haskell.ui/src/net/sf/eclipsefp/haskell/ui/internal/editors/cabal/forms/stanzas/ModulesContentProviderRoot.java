package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import org.eclipse.core.resources.IProject;


public class ModulesContentProviderRoot {
  IProject project;
  PackageDescription description;
  PackageDescriptionStanza stanza;

  public ModulesContentProviderRoot( final IProject project,
      final PackageDescription description, final PackageDescriptionStanza stanza ) {
    super();
    this.project = project;
    this.description = description;
    this.stanza = stanza;
  }

  public IProject getProject() {
    return project;
  }

  public PackageDescription getDescription() {
    return description;
  }

  public PackageDescriptionStanza getStanza() {
    return stanza;
  }
}
