// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// Copyright (c) 2011 by Alejandro Serrano
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.overview;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringWriter;
import java.util.Calendar;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormSection;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.IFormEntryListener;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.LangUtil;
import net.sf.eclipsefp.haskell.util.StreamRedirect;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.variables.IStringVariableManager;
import org.eclipse.core.variables.IValueVariable;
import org.eclipse.core.variables.VariablesPlugin;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.editor.IFormPage;
import org.eclipse.ui.forms.widgets.FormToolkit;

/**
 * <p>
 * form section for legal info (license, copyright ...).
 * </p>
 *
 * @author Leif Frenzel
 */
class LegalSection extends CabalFormSection {
  private FormEntry licenseField;
  private FormEntry fileField;
  private Button bGenerate;
  LicenseFormListener listener;

  LegalSection( final IFormPage page, final Composite parent,
      final CabalFormEditor editor, final IProject project ) {
    super( page, parent, editor, UITexts.legalSection_title, project );
  }

  @Override
  protected void createClient( final FormToolkit toolkit ) {
    Composite container = toolkit.createComposite( getSection() );
    container.setLayout( new GridLayout( 2, false ) );
    GridData data = new GridData( GridData.FILL_BOTH );
    getSection().setLayoutData( data );
    listener=new LicenseFormListener();
    String text = UITexts.legalSection_entryCopyright;
    createFormEntry( CabalSyntax.FIELD_COPYRIGHT, toolkit, container, text );
    String text2 = UITexts.legalSection_entryLicense;
    licenseField=createComboFormEntry( CabalSyntax.FIELD_LICENSE, new LicenseChoice(),
        toolkit, container, text2 );
    String text3 = UITexts.legalSection_entryLicenseFile;
    fileField=createFormEntry( CabalSyntax.FIELD_LICENSE_FILE, toolkit, container, text3 );
    licenseField.addFormEntryListener( listener );
    fileField.addFormEntryListener( listener );

    bGenerate=new Button(container,SWT.PUSH);
    bGenerate.setText( UITexts.legalSection_license_generate );
    bGenerate.addSelectionListener( new GenerateListener() );
    listener.enableButton();
    GridData gd=new GridData(GridData.HORIZONTAL_ALIGN_END);
    gd.horizontalSpan=2;
    bGenerate.setLayoutData( gd );
    toolkit.paintBordersFor( container );
    getSection().setClient( container );
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormSection#fillInValues(boolean)
   */
  @Override
  protected void fillInValues( final boolean first ) {
    super.fillInValues( first );
    listener.enableButton();
  }

  /**
   * listen for changes in license type and file name
   *
   * @author JP Moresmau
   *
   */
  private class LicenseFormListener implements IFormEntryListener{

    /* (non-Javadoc)
     * @see net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.IFormEntryListener#focusGained(net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry)
     */
    @Override
    public void focusGained( final FormEntry entry ) {
      enableButton();

    }

    /* (non-Javadoc)
     * @see net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.IFormEntryListener#textDirty(net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry)
     */
    @Override
    public void textDirty( final FormEntry entry ) {
      enableButton();

    }

    /* (non-Javadoc)
     * @see net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.IFormEntryListener#textValueChanged(net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry)
     */
    @Override
    public void textValueChanged( final FormEntry entry ) {
      enableButton();

    }

    /* (non-Javadoc)
     * @see net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.IFormEntryListener#selectionChanged(net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry)
     */
    @Override
    public void selectionChanged( final FormEntry entry ) {
      enableButton();

    }

    void enableButton(){
      String license=licenseField.getValue();
      String tempFile=null;
      for( License l: License.values() ) {
        if( l.getCabalName().equals( license ) ) {
          tempFile=l.getFileName();
        }
      }
      if (tempFile!=null && tempFile.length()>0){
        bGenerate.setEnabled( true );
      } else {
        bGenerate.setEnabled( false );
      }
    }

  }

  /**
   * generate file
   *
   * @author JP Moresmau
   *
   */
  private class GenerateListener extends SelectionAdapter {
    /* (non-Javadoc)
     * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
     */
    @Override
    public void widgetSelected( final SelectionEvent arg0 ) {
      String license=licenseField.getValue();
      String tempFile=null;
      for( License l: License.values() ) {
        if( l.getCabalName().equals( license ) ) {
          tempFile=l.getFileName();
        }
      }
      String file=((Text)fileField.getControl()).getText();
      if (file==null || file.trim().length()==0){
        file="LICENSE";
        fileField.setValue( file, false );
        setNewValue( file, fileField.getProperty() );
      }
      if (tempFile!=null && tempFile.length()>0){
        InputStream is=getClass().getResourceAsStream( "/licenses/"+tempFile );
        if (is!=null){
          StringWriter sw=new StringWriter();
          StreamRedirect sr=new StreamRedirect( new InputStreamReader( is ), sw );
          sr.run();
          String s=sw.toString();
          IStringVariableManager mgr=VariablesPlugin.getDefault().getStringVariableManager();
          IValueVariable[] vars = new IValueVariable[] {
              mgr.newValueVariable( "year", "", true, String.valueOf(Calendar.getInstance().get( Calendar.YEAR )) ),
              mgr.newValueVariable( "owner", "", true, getStanza().getProperties().get( CabalSyntax.FIELD_AUTHOR.toString() )),
              mgr.newValueVariable( "OWNER", "", true, LangUtil.toUpper( getStanza().getProperties().get( CabalSyntax.FIELD_AUTHOR.toString()))),
          };
          try {
            mgr.addVariables( vars );
            try {
              s=mgr.performStringSubstitution( s );

              mgr.removeVariables( vars );
              IFile f=getProject().getFile( file );
              if (f.exists()){
                f.delete( true, new NullProgressMonitor() );
              }
              f.create( new ByteArrayInputStream( s.getBytes() ), true, new NullProgressMonitor() );
            } finally {
              mgr.removeVariables( vars );
            }
          } catch (CoreException ce){
            HaskellUIPlugin.log( ce );
          }
        }
      }
    }
  }
}
