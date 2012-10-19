/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;


import java.io.File;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.scion.InstallExecutableRunnable;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;


/**
 * An executable field, with autodetect in path functionality
 * code extracted from ScionPP
 * @author JP Moresmau
 *
 */
public class AutodetectExecutableField {
  private final String pgmName;
  private final String fullExeName;
  private final Composite fieldComposite;
  private final ExecutableFileFieldEditor fieldEditor;

//  private final Composite autoComposite;
//  private final ButtonFieldEditor autoEditor;
//  private final Composite installComposite;
//  private final ButtonFieldEditor installEditor;
  private final Button autodetectButton;
  private final Button installButton;
  private final Shell shell;


  public AutodetectExecutableField(final PreferencePage page, final Composite parent,
      final String pgmName,final String exeName,final String prefName,final IPropertyChangeListener listener){
    this.shell=parent.getShell();
    this.pgmName=pgmName;
    fullExeName=FileUtil.makeExecutableName(exeName);
    fieldComposite=new Composite(parent,SWT.NONE);
    GridData gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL | GridData.VERTICAL_ALIGN_CENTER);
    gd.horizontalSpan=2;
    fieldComposite.setLayoutData( gd );
    fieldEditor = new ExecutableFileFieldEditor(prefName,
        NLS.bind(UITexts.executable_label, pgmName, fullExeName),
        false, StringFieldEditor.VALIDATE_ON_KEY_STROKE, fieldComposite ){
      /* (non-Javadoc)
       * @see org.eclipse.jface.preference.StringButtonFieldEditor#getNumberOfControls()
       */
      @Override
      public int getNumberOfControls() {
        return 5;
      }
      /* (non-Javadoc)
           * @see org.eclipse.jface.preference.StringButtonFieldEditor#doFillIntoGrid(org.eclipse.swt.widgets.Composite, int)
           */
        @Override
        protected void doFillIntoGrid( final Composite parent, final int numColumns ) {
          super.doFillIntoGrid( parent, numColumns-2 );
        }
    };

    fieldEditor.setEmptyStringAllowed(true);
    fieldEditor.setPropertyChangeListener( listener);
    fieldEditor.setPage( page );
    fieldEditor.setPreferenceStore( page.getPreferenceStore() );
    fieldEditor.load();

    //new Label(fieldComposite,SWT.NONE);
    //new Label(fieldComposite,SWT.NONE);

    //Composite buttonC=new Composite( fieldComposite, SWT.NONE );
    //buttonC.setLayout( new RowLayout( SWT.HORIZONTAL ) );
    autodetectButton=new Button(fieldComposite,SWT.PUSH);
    autodetectButton.setImage( HaskellUIImages.getImage( IImageNames.AUTODETECT ) );
    autodetectButton.setToolTipText( String.format(UITexts.autodetectButton_label, fullExeName) );
    autodetectButton.addSelectionListener(  new SelectionAdapter() {
          @Override
          public void widgetSelected(final SelectionEvent e) {
            String old=fieldEditor.getStringValue();
            doDetect();
            listener.propertyChange( new PropertyChangeEvent( this, "path", old, fieldEditor.getStringValue() ) );
          }
        } );

//    autoComposite=new Composite(parent,SWT.NONE);
//    gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL | GridData.VERTICAL_ALIGN_CENTER);
//    gd.horizontalSpan=2;
//    autoComposite.setLayoutData( gd );
//    autoEditor = new ButtonFieldEditor(
//        String.format(UITexts.autodetectButton_label, fullExeName),
//        UITexts.autodetectButton_text,
//        new SelectionAdapter() {
//          @Override
//          public void widgetSelected(final SelectionEvent e) {
//            String old=fieldEditor.getStringValue();
//            doDetect();
//            listener.propertyChange( new PropertyChangeEvent( this, "path", old, fieldEditor.getStringValue() ) );
//          }
//        },
//        autoComposite );
//    autoEditor.setPage( page );
//    autoEditor.setPreferenceStore( page.getPreferenceStore() );
//    autoEditor.load();

    installButton=new Button(fieldComposite,SWT.PUSH);
    installButton.setImage( HaskellUIImages.getImage( IImageNames.HACKAGE_INSTALL ) );
    installButton.setToolTipText( String.format(UITexts.installHackageButton_label, exeName) );
    installButton.addSelectionListener(  new SelectionAdapter() {
      @Override
      public void widgetSelected(final SelectionEvent e) {

        installButton.setEnabled( false );
        final String oldVal=fieldEditor.getStringValue();

        final InstallExecutableRunnable r=new InstallExecutableRunnable();
        r.setCabalUpdate( false );
        r.setGlobal( false );
        r.getPackages().add( new InstallExecutableRunnable.Package( exeName, null ) );
        r.setNextRunnable( new Runnable(){
          @Override
          public void run() {
            Runnable r2=new Runnable(){
              /* (non-Javadoc)
               * @see java.lang.Runnable#run()
               */
              @Override
              public void run() {
                if (!shell.isDisposed() && !installButton.isDisposed()){
                  if (r.getErrors().size()>0){
                    MessageDialog.openError(shell,
                        UITexts.installHackageButton_errorTitle,
                        r.getErrors().iterator().next());
                  } else {
                    File f=r.getFiles().get( exeName );
                    if (f!=null){
                      String newVal=f.getAbsolutePath();
                      fieldEditor.setStringValue(newVal);
                      listener.propertyChange( new PropertyChangeEvent( this, "path", oldVal, newVal) ) ;
                      fieldEditor.checkState();
                    }
                  }
                  installButton.setEnabled( true );
                }

              }
            };
            HaskellUIPlugin.getStandardDisplay().asyncExec( r2 );

          }
        } );
        final String title=NLS.bind( UITexts.installExecutableProgress,exeName);
        new Job(title){
          @Override
          protected IStatus run(final IProgressMonitor monitor) {
            monitor.beginTask(title, IProgressMonitor.UNKNOWN);
            try {
              r.run();
            } finally {
              monitor.done();
            }
            return Status.OK_STATUS;
          }
        }.schedule();


      }
    } );

//    installComposite=new Composite(parent,SWT.NONE);
//    gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL | GridData.VERTICAL_ALIGN_CENTER);
//    gd.horizontalSpan=2;
//    installComposite.setLayoutData( gd );
//    installEditor = new ButtonFieldEditor(
//        String.format(UITexts.installHackageButton_label, exeName),
//        UITexts.installHackageButton_text,
//        new SelectionAdapter() {
//          @Override
//          public void widgetSelected(final SelectionEvent e) {
//            final Control c=((Control)e.widget);
//            c.setEnabled( false );
//            final String oldVal=fieldEditor.getStringValue();
//
//            final InstallExecutableRunnable r=new InstallExecutableRunnable();
//            r.setCabalUpdate( false );
//            r.setGlobal( false );
//            r.getPackages().add( new InstallExecutableRunnable.Package( exeName, null ) );
//            r.setNextRunnable( new Runnable(){
//              @Override
//              public void run() {
//                Runnable r2=new Runnable(){
//                  /* (non-Javadoc)
//                   * @see java.lang.Runnable#run()
//                   */
//                  @Override
//                  public void run() {
//                    if (r.getErrors().size()>0){
//                      MessageDialog.openError(shell,
//                          UITexts.installHackageButton_errorTitle,
//                          r.getErrors().iterator().next());
//                    } else {
//                      File f=r.getFiles().get( exeName );
//                      if (f!=null){
//                        String newVal=f.getAbsolutePath();
//                        fieldEditor.setStringValue(newVal);
//                        listener.propertyChange( new PropertyChangeEvent( this, "path", oldVal, newVal) ) ;
//                      }
//                    }
//                    c.setEnabled( true );
//
//                  }
//                };
//                HaskellUIPlugin.getStandardDisplay().asyncExec( r2 );
//
//              }
//            } );
//            final String title=NLS.bind( UITexts.installExecutableProgress,exeName);
//            new Job(title){
//              @Override
//              protected IStatus run(final IProgressMonitor monitor) {
//                monitor.beginTask(title, IProgressMonitor.UNKNOWN);
//                try {
//                  r.run();
//                } finally {
//                  monitor.done();
//                }
//                return Status.OK_STATUS;
//              }
//            }.schedule();
//
//
//          }
//        },
//        installComposite );
//    installEditor.setPage( page );
//    installEditor.setPreferenceStore( page.getPreferenceStore() );
//    installEditor.load();
  }

  private void doDetect(){
    File f=FileUtil.findExecutableInPath( fullExeName );

    if (f == null) {
      MessageDialog.openError(shell,
          UITexts.autodetectButton_errorTitle,
          NLS.bind(UITexts.autodetectButton_errorMessage, pgmName,fullExeName));
    } else {
      fieldEditor.setStringValue(f.getAbsolutePath());
    }
  }

  public void store(){
    fieldEditor.store();
  //  autoEditor.store();
  }

  public boolean isValid(){
    return fieldEditor.getStringValue().length()>0;
  }

  public void setEnabled(final boolean enabled){
    fieldEditor.setEnabled( enabled, fieldComposite );
    //autoEditor.setEnabled( enabled, autoComposite );
    autodetectButton.setEnabled( enabled );
    installButton.setEnabled( enabled );
  }
}
