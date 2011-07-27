/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import org.eclipse.core.resources.IProject;


public class FormEntryModulesRoot {
  IProject project;
  PackageDescription description;
  PackageDescriptionStanza stanza;

  public FormEntryModulesRoot( final IProject project,
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
