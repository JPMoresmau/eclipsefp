// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// Copyright (c) 2011 by Alejandro Serrano
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormSection;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntryCombo;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.IFormEntryListener;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.editor.IFormPage;
import org.eclipse.ui.forms.widgets.FormToolkit;

/**
 * <p>
 * form section for selecting test type
 * </p>
 *
 * @author Alejandro Serrano
 */
class TestTypeSection extends CabalFormSection implements IFormEntryListener {

  FormEntryCombo<TestSuiteType> choice;
  FormEntry checkbox;

  TestTypeSection( final IFormPage page, final Composite parent,
      final CabalFormEditor editor, final IProject project ) {
    super( page, parent, editor, UITexts.cabalEditor_testType, project );
  }

  @Override
  protected void createClient( final FormToolkit toolkit ) {
    Composite container = toolkit.createComposite( getSection() );
    container.setLayout( new GridLayout( 1, false ) );
    GridData data = new GridData( GridData.FILL_HORIZONTAL );
    getSection().setLayoutData( data );

    choice = new FormEntryCombo<TestSuiteType>( new TestSuiteTypeChoice() );
    setCustomFormEntry( choice, CabalSyntax.FIELD_TYPE, toolkit, container );
    GridData entryGD = new GridData( GridData.FILL_HORIZONTAL );
    entryGD.heightHint = 20;
    choice.getControl().setLayoutData( entryGD );

    choice.addFormEntryListener( this );

    checkbox = createCheckBoxEntry( CabalSyntax.FIELD_X_USES_TEST_FRAMEWORK,
        UITexts.cabalEditor_isTestFrameworkTestSuite, toolkit, container );

    toolkit.paintBordersFor( container );
    getSection().setClient( container );
  }

  public void textValueChanged( final FormEntry entry ) {
    @SuppressWarnings ( "unchecked" )
    FormEntryCombo<TestSuiteType> fEntry = ( FormEntryCombo<TestSuiteType> )entry;
    CabalSyntax deletedElement = null, addedElement = null;
    if( fEntry.getValue()
        .equals( CabalSyntax.VALUE_DETAILED_0_9.getCabalName() ) ) {
      deletedElement = CabalSyntax.FIELD_MAIN_IS;
      addedElement = CabalSyntax.FIELD_TEST_MODULE;

    } else if( fEntry.getValue().equals(
        CabalSyntax.VALUE_EXITCODE_STDIO_1_0.getCabalName() ) ) {
      deletedElement = CabalSyntax.FIELD_TEST_MODULE;
      addedElement = CabalSyntax.FIELD_MAIN_IS;
    }
    PackageDescriptionStanza stanza = this.getStanza();
    if( stanza.getProperties().containsKey( deletedElement.getCabalName() ) ) {
      String value = stanza.getProperties().get( deletedElement.getCabalName() );
      setNewValue( value, addedElement );
      setNewValue( "", deletedElement );
    }
  }

  public void focusGained( final FormEntry entry ) {
    // Do nothing
  }

  public void textDirty( final FormEntry entry ) {
    // Do nothing
  }

  public void selectionChanged( final FormEntry entry ) {
    // Do nothing
  }
}
