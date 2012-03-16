/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.scion;

import java.io.File;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementationManager;
import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;
import net.sf.eclipsefp.haskell.core.util.GHCSyntax;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.ProcessRunner;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.osgi.util.NLS;


/**
 * this runnable runs cabal update, then installs buildwrapper and/or scion-browser
 * @author JP Moresmau
 *
 */
public class InstallExecutableRunnable implements Runnable {
  private boolean cabalUpdate=true;
  private boolean buildWrapper=true;
  private boolean scionBrowser=true;
  private boolean global=true;



  public InstallExecutableRunnable( ) {
    super( );
  }

  @Override
  public void run(){
    final String cabalExecutable=CabalImplementationManager.getCabalExecutable();
    final File folder=new File(cabalExecutable).getParentFile();
    final LinkedList<Command> commands=new LinkedList<Command>();

    File binDir=new File(CompilerManager.getInstance().getCurrentHsImplementation().getBinDir());
    if (!global){
      File exe=new File(binDir,GHCSyntax.GHC);
      StringWriter sw=new StringWriter();
      try {
        // we run ghc in execution mode to find the directory cabal is going to use
        new ProcessRunner().executeBlocking( binDir, sw, null, exe.getAbsolutePath(),"-e","\"System.Directory.getAppUserDataDirectory \\\"cabal\\\"\"" );
        String s=sw.toString().trim(); // line return at end
        if (s.startsWith( "\"" )){ // quotes
          s=s.substring( 1 );
        }
        if (s.endsWith( "\"" )){// quotes
          s=s.substring( 0,s.length()-1 );
        }
        binDir=new File(s,"bin");
      } catch (Exception e){
        HaskellUIPlugin.log( e );
        return;
      }
    }

    if (cabalUpdate){
      commands.add(new Command(UITexts.cabalUpdateProgress,Arrays.asList( cabalExecutable , "update" )));
    }
    if (buildWrapper){
      commands.add(new Command(UITexts.builWrapperInstallProgress,"buildwrapper",IPreferenceConstants.BUILDWRAPPER_EXECUTABLE,Arrays.asList( cabalExecutable , "install","buildwrapper", global?"--global": "--user" )));
    }
    if (scionBrowser){
      commands.add(new Command(UITexts.scionBrowserInstallProgress,"scion-browser",IPreferenceConstants.SCION_BROWSER_SERVER_EXECUTABLE,Arrays.asList( cabalExecutable , "install","scion-browser", global?"--global": "--user" )));
    }
    final File fBinDir=binDir;

    Runnable r=new Runnable(){
      @Override
      public void run() {
        if (commands.size()>0){
          final Command c=commands.removeFirst();
          final Runnable orig=this;
          Runnable next=this;
          if (c.exeName!=null){
            next=new Runnable() {

              @Override
              public void run() {
                File f=new File(fBinDir,FileUtil.makeExecutableName( c.exeName ));
                if (f.exists()){
                  // set preference
                  HaskellUIPlugin.getDefault().getPreferenceStore().setValue(c.prefName,f.getAbsolutePath());
                } else {
                  // oops, write message
                  HaskellUIPlugin.log( NLS.bind( UITexts.installExecutableMissing, f.getAbsolutePath() ), IStatus.ERROR );
                }
                orig.run();
              }
            };

          }
          final Runnable fNext=next;
          HaskellUIPlugin.getStandardDisplay().asyncExec(new Runnable() {

            @Override
            public void run() {
              try {
                AbstractHaskellLaunchDelegate.runInConsole( null, c.commands, folder, c.title, true,fNext );
              } catch (CoreException ce){
                HaskellUIPlugin.log( ce );
              }

            }
          });

        }
      }
    };
    HaskellUIPlugin.getStandardDisplay().asyncExec( r );
    //r.run();
  }

  public boolean isCabalUpdate() {
    return cabalUpdate;
  }

  public void setCabalUpdate( final boolean cabalUpdate ) {
    this.cabalUpdate = cabalUpdate;
  }

  public boolean isBuildWrapper() {
    return buildWrapper;
  }

  public void setBuildWrapper( final boolean buildWrapper ) {
    this.buildWrapper = buildWrapper;
  }

  public boolean isScionBrowser() {
    return scionBrowser;
  }

  public void setScionBrowser( final boolean scionBrowser ) {
    this.scionBrowser = scionBrowser;
  }

  public boolean isGlobal() {
    return global;
  }

  public void setGlobal( final boolean global ) {
    this.global = global;
  }

  /**
   * internal structure for a command
   *
   */
  private class Command {
    /**
     * title to display on top of console
     */
      private final String title;
      /**
       * commands to run
       */
      private final List<String> commands;
      /**
       * name of exe built, if any
       */
      private String exeName;
      /**
       * preference to set at end, if any
       */
      private String prefName;

      public Command( final String title, final List<String> commands ) {
        super();
        this.title = title;
        this.commands = commands;
      }

      public Command( final String title, final String exeName,
          final String prefName, final List<String> commands ) {
        super();
        this.title = title;
        this.commands = commands;
        this.exeName = exeName;
        this.prefName = prefName;
      }
  }

}
