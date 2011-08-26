/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.uuagcOptions;

import net.sf.eclipsefp.haskell.core.uuagc.UuagcFile;
import net.sf.eclipsefp.haskell.core.uuagc.UuagcProjectManager;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.ui.forms.IFormPart;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.editor.FormPage;
import org.eclipse.ui.forms.widgets.ColumnLayout;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;

/**
 * Visual editor for uuagc_options page.
 * @author Alejandro Serrano
 *
 */
public class UuagcOptionsPage extends FormPage {

  protected IProject project;
  private final UuagcProjectManager manager;
  private final UuagcOptionsFormEditor theEditor;
  private IManagedForm theManagedForm;

  public UuagcOptionsPage( final UuagcOptionsFormEditor editor,
      final IProject project ) {
    super( editor, UuagcOptionsPage.class.getName(),
        UITexts.uuagcEditor_uuagcOptions );
    this.theEditor = editor;
    this.project = project;
    this.manager = new UuagcProjectManager( project );
  }

  @Override
  protected void createFormContent( final IManagedForm managedForm ) {
    ScrolledForm form = managedForm.getForm();
    FormToolkit toolkit = managedForm.getToolkit();
    toolkit.decorateFormHeading( form.getForm() );
    form.updateToolBar();
    form.setText( UITexts.uuagcEditor_uuagcOptions );
    form.getBody().setLayout( new ColumnLayout() );

    createContentsFromManager();

    theEditor.getModel().addDocumentListener( new IDocumentListener() {

      public void documentAboutToBeChanged( final DocumentEvent event ) {
        // Nothing
      }

      public void documentChanged( final DocumentEvent event ) {
        // Nothing
        String pastContents = manager.toUuagcString();
        String docContents = theEditor.getModel().get();
        if (!docContents.equals( pastContents )) {
          createContentsFromManager();
        }
      }
    } );
  }

  private void createContentsFromManager() {
    IManagedForm mForm = getManagedForm();
    ScrolledForm form = mForm.getForm();
    for (IFormPart part : mForm.getParts()) {
      UuagcFileSection sPart = (UuagcFileSection)part;
      sPart.getSection().dispose();
      sPart.getInnerComposite().dispose();
      mForm.removePart( part );
    }

    this.manager.initFromContents( theEditor.getModel().get() );
    for( UuagcFile file: manager.getElements() ) {
      UuagcFileSection section = new UuagcFileSection( this, form.getBody(),
          file );
      mForm.addPart( section );
    }
  }

  void makeChanges() {
    String newContents = manager.toUuagcString();
    String docContents = theEditor.getModel().get();
    if (!newContents.equals( docContents )) {
      theEditor.getModel().set( newContents );
    }
  }
}
