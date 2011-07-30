// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
// Copyright (c) 2011 by Alejandro Serrano
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import java.util.ArrayList;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.cabalmodel.RealValuePosition;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.SectionPart;
import org.eclipse.ui.forms.editor.IFormPage;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;

/**
 * <p>
 * form section super class - encapsulates some common functionality.
 * </p>
 *
 * @author Leif Frenzel, serras
 */
public abstract class CabalFormSection extends SectionPart {

  protected final CabalFormEditor editor;
  protected PackageDescriptionStanza stanza = null;
  protected final ArrayList<FormEntry> entries;
  private final IProject project;

  protected CabalFormSection( final IFormPage page, final Composite parent,
      final CabalFormEditor editor, final String text, final IProject project ) {
    super( parent, page.getManagedForm().getToolkit(), Section.DESCRIPTION
        | ExpandableComposite.TITLE_BAR );
    this.editor = editor;
    entries = new ArrayList<FormEntry>();
    this.project = project;
    initialize( page.getManagedForm() );
    getSection().clientVerticalSpacing = 6;
    getSection().setData( "part", this );

    getSection().setText( text );
    createClient( page.getManagedForm().getToolkit() );
    fillInValues( true );
    editor.getModel().addDocumentListener( new IDocumentListener() {

      public void documentAboutToBeChanged( final DocumentEvent event ) {
        // unused
      }

      public void documentChanged( final DocumentEvent event ) {
        fillInValues( false );
      }
    } );
  }

  protected abstract void createClient( FormToolkit toolkit );

  protected IProject getProject() {
    return project;
  }

  public PackageDescriptionStanza getStanza() {
    return stanza;
  }

  public void setStanza( final PackageDescriptionStanza stanza ) {
    this.stanza = stanza;
    fillInValues( false );
    this.setAllEditable( stanza != null );
  }

  IStatusLineManager getStatusLineManager() {
    return editor.getEditorSite().getActionBars().getStatusLineManager();
  }

  protected FormEntry createCustomFormEntry(final FormEntry customEntry, final CabalSyntax property,
      final FormToolkit toolkit, final Composite container, final String label, final boolean showLabelAbove, final int style) {
    FormEntryDecorator decorator = new FormEntryDecorator( label, showLabelAbove, customEntry );
    decorator.init( project, container, toolkit, style );
    decorator.setProperty( property );
    decorator.addFormEntryListener( createFormEntryListener() );
    entries.add( decorator );
    return decorator;
  }

  protected FormEntry createCustomFormEntry(final FormEntry customEntry, final CabalSyntax property,
      final FormToolkit toolkit, final Composite container, final String label, final int style) {
    return createCustomFormEntry( customEntry, property, toolkit, container, label, false, style );
  }

  protected FormEntry createFormEntry( final CabalSyntax property,
      final FormToolkit toolkit, final Composite container, final String label ) {
    return createCustomFormEntry( new FormEntryText(), property, toolkit, container, label, SWT.NONE );
  }

  protected FormEntry createMultiLineFormEntry( final CabalSyntax property,
      final FormToolkit toolkit, final Composite container, final String label ) {
    return createCustomFormEntry( new FormEntryText(), property, toolkit, container, label, SWT.MULTI );
  }

  protected <T> FormEntry createComboFormEntry( final CabalSyntax property,
      final Choice<T> choices, final FormToolkit toolkit,
      final Composite container, final String label ) {
    return createCustomFormEntry( new FormEntryCombo<T>( choices ), property, toolkit, container, label, SWT.NONE );
  }

  protected FormEntry createFileFormEntry (final CabalSyntax property, final FormToolkit toolkit, final Composite container) {
    FormEntryFile entry = new FormEntryFile();
    setCustomFormEntry( entry, property, toolkit, container );
    return entry;
  }

  protected FormEntry createDirFormEntry (final CabalSyntax property, final FormToolkit toolkit, final Composite container) {
    FormEntryFile entry = new FormEntryFile(true);
    setCustomFormEntry( entry, property, toolkit, container );
    return entry;
  }

  protected FormEntry createCheckBoxEntry (final CabalSyntax property, final String title, final FormToolkit toolkit, final Composite container) {
    FormEntryCheckBox entry = new FormEntryCheckBox( title );
    setCustomFormEntry( entry, property, toolkit, container );
    return entry;
  }

  protected void setCustomFormEntry (final FormEntry entry, final CabalSyntax property, final FormToolkit toolkit, final Composite container) {
    entry.init( project, container, toolkit, SWT.NONE );
    entry.setProperty( property );
    entry.addFormEntryListener( createFormEntryListener() );
    entries.add( entry );
  }

  // helping functions
  // //////////////////

  protected void fillInValues( final boolean first ) {
    // try {
    setAllEditable( true );
    getStatusLineManager().setErrorMessage( null );
    for( FormEntry entry: entries ) {
      fillValue( entry, entry.getProperty(), first );
    }
    /*
     * } catch( final InvalidCabalFileException icfex ) { setAllEditable( false
     * ); getStatusLineManager().setErrorMessage( icfex.getMessage() ); } catch(
     * final CoreException cex ) { HaskellUIPlugin.log( cex ); }
     */
  }

  protected void fillValue( final FormEntry entry, final CabalSyntax acc,
      final boolean first ) {
    if( stanza != null ) {
      String value = stanza.getProperties().get( acc );
      entry.setValue( value, first );
    } else {
      entry.setValue( "", first ); //$NON-NLS-1$
    }
  }

  public void setAllEditable( final boolean editable ) {
    for( FormEntry entry: entries ) {
      entry.setEditable( editable );
    }
  }

  protected void setNewValue( final FormEntry text, final CabalSyntax mutator ) {
    setNewValue( text.getValue(), mutator );
  }

  protected void setNewValue( final String newValue, final CabalSyntax mutator ) {
    if (this.stanza != null) {
      String oldValue = stanza.getProperties().get( mutator.getCabalName() );
      oldValue = oldValue == null ? "" : oldValue;

      if (!newValue.equals( oldValue )) {
        stanza.getProperties().put( mutator.getCabalName(), newValue );
        RealValuePosition vp = stanza.update( mutator, newValue );
        vp.updateDocument( editor.getModel() );
      }
    }
  }

  private IFormEntryListener createFormEntryListener() {
    return new FormEntryAdapter() {

      @Override
      public void textValueChanged( final FormEntry formEntry ) {
        setNewValue( formEntry, formEntry.getProperty() );
      }
    };
  }

}
