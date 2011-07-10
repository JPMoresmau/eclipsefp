package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import java.util.List;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.scion.types.Component.ComponentType;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.ui.forms.editor.FormEditor;


public class TestSuitesPage extends ExecutablesTestSuitePage {

  public TestSuitesPage( final FormEditor editor, final IProject project ) {
    super( editor, project, UITexts.cabalEditor_testSuites );
  }

  @Override
  public String getMessage( final Messages m ) {
    switch(m) {
      case TITLE:
        return UITexts.cabalEditor_testSuites;
      case NEW:
        return UITexts.cabalEditor_newTestSuiteString;
      case BLANK_ERROR:
        return UITexts.cabalEditor_newTestSuiteBlankError;
      case ALREADY_EXISTS_ERROR:
        return UITexts.cabalEditor_newTestSuiteAlreadyExistsError;
    }
    return null;
  }

  @Override
  public PackageDescriptionStanza createNewStanza(final PackageDescription desc, final String name) {
    PackageDescriptionStanza stanza = desc.addStanza(
        CabalSyntax.SECTION_TESTSUITE, name );
    stanza.setIndent( 2 );
    stanza.update( CabalSyntax.FIELD_TYPE, CabalSyntax.VALUE_EXITCODE_STDIO_1_0.getCabalName() );
    return stanza;
  }

  @Override
  public List<PackageDescriptionStanza> getStanzas(
      final PackageDescription description ) {
    return description.getTestSuiteStanzas();
  }

  @Override
  public ComponentType getComponentType() {
    return ComponentType.TESTSUITE;
  }

}
