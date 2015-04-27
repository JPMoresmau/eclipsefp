/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.wizards.cabal;

import java.util.Collection;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Text;


/**
 * Options for cabal haddock export wizard
 * @author JP Moresmau
 *
 */
public class CabalHaddockOptionsPage extends WizardPage  {
  private static boolean doHoogle=true; // since we can use Hoogle data from our own projects, default to true
  private static boolean doExecutables=false;
  private static boolean doInternal=false;
  private static boolean doColourSrc=false;
  private static boolean doCss=false;
  private static String css="";
  private static boolean doColourSrcCss=false;
  private static String colourSrcCss="";

  private static boolean doHtml=false;
  private static String html="https://hackage.haskell.org/packages/archive/$pkg/latest/doc/html";

  private DistFolder dFolder;
  private final Collection<IProject> projects;

  public CabalHaddockOptionsPage(final Collection<IProject> projects) {
    super( "HaddockOptions", UITexts.exportDoc_options, null );
    this.projects=projects;
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent ) {
    initializeDialogUnits( parent );
    Composite composite = new Composite( parent, SWT.NONE );
    GridData gd=new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL);
    composite.setLayoutData( gd );


    GridLayout layout=new GridLayout( 3, false );
    composite.setLayout( layout );

    dFolder=new DistFolder(projects,composite, UITexts.exportSource_options_folder,UITexts.exportSource_options_folder_choose,UITexts.exportSource_options_folder_choose );

    Composite optionsComposite=new Composite(composite, SWT.NONE);
    GridData gdOptions=new GridData(GridData.HORIZONTAL_ALIGN_FILL);
    gdOptions.horizontalSpan=3;
    optionsComposite.setLayoutData( gdOptions );
    optionsComposite.setLayout(new GridLayout( 1, false ));

    final Button bHoogle=new Button(optionsComposite, SWT.CHECK);
    bHoogle.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_FILL) );
    bHoogle.setText( UITexts.exportDoc_options_hoogle );
    bHoogle.setSelection( doHoogle );
    bHoogle.addSelectionListener( new SelectionAdapter() {
      /* (non-Javadoc)
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e ) {
         doHoogle=bHoogle.getSelection();
      }
    } );

    final Button bExecutables=new Button(optionsComposite, SWT.CHECK);
    bExecutables.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_FILL) );
    bExecutables.setText( UITexts.exportDoc_options_executables );
    bExecutables.setSelection( doExecutables );
    bExecutables.addSelectionListener( new SelectionAdapter() {
      /* (non-Javadoc)
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        doExecutables=bExecutables.getSelection();
      }
    } );

    final Button bInternal=new Button(optionsComposite, SWT.CHECK);
    bInternal.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_FILL) );
    bInternal.setText( UITexts.exportDoc_options_internal );
    bInternal.setSelection( doInternal );
    bInternal.addSelectionListener( new SelectionAdapter() {
      /* (non-Javadoc)
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        doInternal=bInternal.getSelection();
      }
    } );

    final Button bHTML=new Button(optionsComposite, SWT.CHECK);
    bHTML.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_FILL) );
    bHTML.setText( UITexts.exportDoc_options_html );
    bHTML.setSelection( doHtml );

    final Text tHTML=new Text(optionsComposite,SWT.BORDER);
    GridData gdHTML=new GridData(GridData.HORIZONTAL_ALIGN_FILL);
    gdHTML.horizontalIndent=20;
    tHTML.setLayoutData( gdHTML );
    tHTML.setText( html );
    tHTML.setEnabled( doHtml );

    bHTML.addSelectionListener( new SelectionAdapter() {
      /* (non-Javadoc)
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        doHtml=bHTML.getSelection();
        tHTML.setEnabled( doHtml );
      }
    } );

    final Button bCSS=new Button(optionsComposite, SWT.CHECK);
    bCSS.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_FILL) );
    bCSS.setText( UITexts.exportDoc_options_css );
    bCSS.setSelection( doCss );

    final Composite cCSS=new Composite(optionsComposite,SWT.NONE);
    GridData gdCSS=new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL);
    gdCSS.horizontalIndent=20;
    cCSS.setLayoutData( gdCSS );
    cCSS.setLayout( new GridLayout(2,false) );

    final Text tCSS=new Text(cCSS,SWT.BORDER);
    tCSS.setText( css );
    tCSS.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL) );
    tCSS.setEnabled( doCss );

    final Button bfCSS=new Button(cCSS,SWT.PUSH);
    bfCSS.setText( "..." );
    bfCSS.setEnabled( doCss );

    bCSS.addSelectionListener( new SelectionAdapter() {
      /* (non-Javadoc)
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        doCss=bCSS.getSelection();
        tCSS.setEnabled( doCss );
        bfCSS.setEnabled( doCss );
      }
    } );
    bfCSS.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected(final SelectionEvent e) {
        FileDialog fd=new FileDialog( getShell() ,SWT.OPEN);
        fd.setFilterExtensions( new String[]{"*.css","*.*"} );
        fd.setFilterNames(new String[]{UITexts.exportDoc_options_filter_css,UITexts.exportDoc_options_filter_all} );
        if (css.length()>0){
          fd.setFileName( css );
        }
        String f=fd.open();
        if (f!=null){
          css=f;
          tCSS.setText( css );
        }
      }
    } );

    final Button bColour=new Button(optionsComposite, SWT.CHECK);
    bColour.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_FILL) );
    bColour.setText( UITexts.exportDoc_options_hscolour );
    bColour.setSelection( doColourSrc );


    final Button bColourCSS=new Button(optionsComposite, SWT.CHECK);
    bColourCSS.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_FILL) );
    bColourCSS.setText( UITexts.exportDoc_options_hscolour_css );
    bColourCSS.setEnabled( doColourSrc );
    bColourCSS.setSelection( doColourSrcCss );

    final Composite cColourCSS=new Composite(optionsComposite,SWT.NONE);
    GridData gdColourCSS=new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL);
    gdColourCSS.horizontalIndent=20;
    cColourCSS.setLayoutData( gdColourCSS );
    cColourCSS.setLayout( new GridLayout(2,false) );

    final Text tColourCSS=new Text(cColourCSS,SWT.BORDER);
    tColourCSS.setText( colourSrcCss );
    tColourCSS.setEnabled( doColourSrc && doColourSrcCss );
    tColourCSS.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL) );

    final Button bfColourCSS=new Button(cColourCSS,SWT.PUSH);
    bfColourCSS.setText( "..." );
    bfColourCSS.setEnabled( doColourSrc && doColourSrcCss );

    bColourCSS.addSelectionListener( new SelectionAdapter() {
      /* (non-Javadoc)
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        doColourSrcCss=bColourCSS.getSelection();
        tColourCSS.setEnabled( doColourSrcCss );
        bfColourCSS.setEnabled( doColourSrcCss );
      }
    } );
    bfColourCSS.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected(final SelectionEvent e) {
        FileDialog fd=new FileDialog( getShell() ,SWT.OPEN);
        fd.setFilterExtensions( new String[]{"*.css","*.*"} );
        fd.setFilterNames(new String[]{UITexts.exportDoc_options_filter_css,UITexts.exportDoc_options_filter_all} );

        if (colourSrcCss.length()>0){
          fd.setFileName( colourSrcCss );
        }
        String f=fd.open();
        if (f!=null){
          colourSrcCss=f;
          tColourCSS.setText( colourSrcCss );
        }
      }
    } );


    bColour.addSelectionListener( new SelectionAdapter() {
      /* (non-Javadoc)
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        doColourSrc=bColour.getSelection();
        if (!doColourSrc){
          bColourCSS.setSelection( false );
        }
        bColourCSS.setEnabled( doColourSrc );
        tColourCSS.setEnabled( doColourSrc && doColourSrcCss );
        bfColourCSS.setEnabled( doColourSrc && doColourSrcCss );
      }
    } );



    setControl( composite );
    Dialog.applyDialogFont( composite );
  }

  public String getFolder(){
    return dFolder.getFolder();
  }


  /**
   * @return the doHoogle
   */
  public static boolean isDoHoogle() {
    return doHoogle;
  }

  public static boolean isDoExecutables() {
    return doExecutables;
  }


  public static boolean isDoInternal() {
    return doInternal;
  }


  public static boolean isDoColourSrc() {
    return doColourSrc;
  }


  public static String getCss() {
    return css;
  }


  public static String getColourSrcCss() {
    return colourSrcCss;
  }


  public static String getHtml() {
    return html;
  }


  public static boolean isDoCss() {
    return doCss;
  }


  public static boolean isDoColourSrcCss() {
    return doColourSrcCss;
  }


  public static boolean isDoHtml() {
    return doHtml;
  }

}
