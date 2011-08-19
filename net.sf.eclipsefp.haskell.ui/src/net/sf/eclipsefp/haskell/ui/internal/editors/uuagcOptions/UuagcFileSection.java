package net.sf.eclipsefp.haskell.ui.internal.editors.uuagcOptions;

import net.sf.eclipsefp.haskell.core.uuagc.UuagcFile;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.SectionPart;
import org.eclipse.ui.forms.widgets.ColumnLayout;
import org.eclipse.ui.forms.widgets.ColumnLayoutData;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;


public class UuagcFileSection extends SectionPart {

  UuagcFile file;
  UuagcOptionsPage page;
  Composite c;

  public UuagcFileSection( final UuagcOptionsPage page, final Composite parent,
      final UuagcFile file ) {
    super( parent, page.getManagedForm().getToolkit(), Section.DESCRIPTION
        | ExpandableComposite.TITLE_BAR );
    this.page = page;
    this.file = file;
    initialize( page.getManagedForm() );
    // Set file name as title
    getSection().setText( file.getFilename() );

    ScrolledForm form = getManagedForm().getForm();

    c = getManagedForm().getToolkit()
        .createComposite( form.getBody() );
    ColumnLayout layout = new ColumnLayout();
    layout.maxNumColumns = 2;
    c.setLayout( layout );

    createCheckbox( c, "haskellsyntax", UITexts.uuagcEditor_haskellSyntaxDescr );
    createCheckbox( c, "data", UITexts.uuagcEditor_dataDescr );
    createCheckbox( c, "semfuns", UITexts.uuagcEditor_semfunsDescr );
    createCheckbox( c, "catas", UITexts.uuagcEditor_catasDescr );
    createCheckbox( c, "signatures", UITexts.uuagcEditor_signaturesDescr );
    createCheckbox( c, "newtypes",
        UITexts.uuagcEditor_newtypesDescr );
    createCheckbox( c, "pretty", UITexts.uuagcEditor_prettyDescr );
    createCheckbox( c, "wrappers", UITexts.uuagcEditor_wrappersDescr );
    createCheckbox( c, "rename", UITexts.uuagcEditor_renameDescr );
    createCheckbox( c, "nest", UITexts.uuagcEditor_nestDescr );
    createCheckbox( c, "self", UITexts.uuagcEditor_selfDescr );
    createCheckbox( c, "cycle", UITexts.uuagcEditor_cycleDescr );
  }

  void createCheckbox( final Composite parent, final String option,
      final String text ) {
    final Button b = getManagedForm().getToolkit().createButton( parent, text,
        SWT.CHECK );
    b.setSelection( file.hasOption( option ) );
    ColumnLayoutData data = new ColumnLayoutData();
    data.heightHint = 15;
    b.setLayoutData( data );
    b.addSelectionListener( new SelectionListener() {

      public void widgetSelected( final SelectionEvent e ) {
        if( b.getSelection() ) {
          file.addOption( option );
        } else {
          file.removeOption( option );
        }
        page.makeChanges();
      }

      public void widgetDefaultSelected( final SelectionEvent e ) {
        // Nothing here
      }
    } );
  }

  Composite getInnerComposite() {
    return c;
  }
}
