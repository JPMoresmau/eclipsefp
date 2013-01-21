/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.scion;

import java.io.File;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementationManager;
import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;
import net.sf.eclipsefp.haskell.core.util.GHCSyntax;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.views.CabalPackagesView;
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
  //private boolean buildWrapper=true;
  //private boolean scionBrowser=true;
  private boolean global=false;
  private final List<Package> packages=new ArrayList<Package>();
  private Runnable nextRunnable;
  private final List<String> errors=new ArrayList<String>();
  private final Map<String,File> files=new HashMap<String, File>();

  public InstallExecutableRunnable( ) {
    super( );
  }

  private void logError(final String msg){
    errors.add(msg);
    HaskellUIPlugin.log( msg, IStatus.ERROR );
  }

  private void logError(final Throwable t){
    errors.add(t.getLocalizedMessage());
    HaskellUIPlugin.log( t);
  }

  @Override
  public void run(){
    errors.clear();
    files.clear();
    final String cabalExecutable=CabalImplementationManager.getCabalExecutable();
    if (cabalExecutable==null){
      logError( UITexts.noCabalImplementationForInstall_error);
      if (getNextRunnable()!=null){
        getNextRunnable().run();
      }
      return;
    }
    final File folder=new File(cabalExecutable).getParentFile();
    final LinkedList<Command> commands=new LinkedList<Command>();

    File binDir=new File(CompilerManager.getCurrentHsImplementation().getBinDir());
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
        logError( e );
        if (getNextRunnable()!=null){
          getNextRunnable().run();
        }
        return;
      }
    }

    if (cabalUpdate){
      commands.add(new Command(UITexts.cabalUpdateProgress,Arrays.asList( cabalExecutable , "update" )));
    }
    /*
    if (buildWrapper){
      commands.add(new Command(UITexts.builWrapperInstallProgress,"buildwrapper",IPreferenceConstants.BUILDWRAPPER_EXECUTABLE,Arrays.asList( cabalExecutable , "install","buildwrapper", global?"--global": "--user" )));
    }
    if (scionBrowser){
      commands.add(new Command(UITexts.scionBrowserInstallProgress,"scion-browser",IPreferenceConstants.SCION_BROWSER_SERVER_EXECUTABLE,Arrays.asList( cabalExecutable , "install","scion-browser", global?"--global": "--user" )));
    }*/
    for (Package p:packages){
      List<String> args=new ArrayList<String>(Arrays.asList( cabalExecutable , "install",p.getName(), global?"--global": "--user" ));

      File f=new File(binDir,FileUtil.makeExecutableName( p.getName() ));
      if (!f.exists()){ // the exe does not exist, we force reinstall to make sure it wasn't deleted manually
        args.add( "--reinstall" );
      }
      commands.add(new Command(NLS.bind( UITexts.installExecutableProgress,p.getName()),p.getName(),p.getPreference(),args));
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
                /** refresh the cabal packages view **/
                CabalPackagesView.refresh();

                File f=new File(fBinDir,FileUtil.makeExecutableName( c.exeName ));
                if (f.exists()){
                  // set preference
                  if (c.prefName!=null){
                    HaskellUIPlugin.getDefault().getPreferenceStore().setValue(c.prefName,f.getAbsolutePath());
                  }
                  files.put( c.exeName, f);
                } else {
                  // oops, write message
                  logError( NLS.bind( UITexts.installExecutableMissing, f.getAbsolutePath() ) );
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
                logError( ce );
              }

            }
          });

        } else {
          if (nextRunnable!=null){
            nextRunnable.run();
          }
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

//  public boolean isBuildWrapper() {
//    return buildWrapper;
//  }
//
//  public void setBuildWrapper( final boolean buildWrapper ) {
//    this.buildWrapper = buildWrapper;
//  }
//
//  public boolean isScionBrowser() {
//    return scionBrowser;
//  }
//
//  public void setScionBrowser( final boolean scionBrowser ) {
//    this.scionBrowser = scionBrowser;
//  }

  public boolean isGlobal() {
    return global;
  }

  public void setGlobal( final boolean global ) {
    this.global = global;
  }

  /**
   * @return the packages
   */
  public List<Package> getPackages() {
    return packages;
  }


  public Runnable getNextRunnable() {
    return nextRunnable;
  }


  public void setNextRunnable( final Runnable nextRunnable ) {
    this.nextRunnable = nextRunnable;
  }



  /**
   * @return the files
   */
  public Map<String, File> getFiles() {
    return files;
  }


  /**
   * @return the errors
   */
  public List<String> getErrors() {
    return errors;
  }

  /**
   * internal structure for a command
   *
   */
  private static class Command {
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

  public static class Package {
    private String name;
    private String preference;



    public Package( final String name, final String preference ) {
      super();
      this.name = name;
      this.preference = preference;
    }

    public String getName() {
      return name;
    }

    public void setName( final String name ) {
      this.name = name;
    }


    public String getPreference() {
      return preference;
    }

    public void setPreference( final String preference ) {
      this.preference = preference;
    }


  }




}
