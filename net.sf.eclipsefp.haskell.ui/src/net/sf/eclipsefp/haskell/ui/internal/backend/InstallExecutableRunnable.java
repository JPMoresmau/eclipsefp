/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.backend;

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
  private final List<Package> packages=new ArrayList<>();
  private Runnable nextRunnable;
  private final List<String> errors=new ArrayList<>();
  private final Map<String,File> files=new HashMap<>();

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
    File folder=new File(cabalExecutable).getParentFile();
    final LinkedList<Command> commands=new LinkedList<>();

    File binDir=new File(CompilerManager.getCurrentHsImplementation().getBinDir());
    if (cabalUpdate){
      commands.add(new Command(UITexts.cabalUpdateProgress,Arrays.asList( cabalExecutable , "update" )));
    }

    if (CabalImplementationManager.getInstance().getDefaultCabalImplementation().allowsSandbox()){
      folder=new File(HaskellUIPlugin.getDefault().getStateLocation().append( "sandbox" ).toOSString());
      folder.mkdirs();
      commands.add(new Command(UITexts.cabalInitProgress,Arrays.asList( cabalExecutable , "sandbox","init","--sandbox="+ folder )));
      binDir=new File(folder,"bin");

    } else {
      if (!global){
        File exe=new File(binDir,GHCSyntax.GHC);
        StringWriter sw=new StringWriter();
        try {
          // we run ghc in execution mode to find the directory cabal is going to use
          // no quotes around last arguments, as it breaks unixes (returns the command, as I suppose ghc evaluates the argument as a string value)
          // windows: use old version!
          new ProcessRunner().executeBlocking( binDir, sw, null, exe.getAbsolutePath(),"-e",ProcessRunner.getGHCArgument( "System.Directory.getAppUserDataDirectory \"cabal\"" ));
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
    }


    /*
    if (buildWrapper){
      commands.add(new Command(UITexts.builWrapperInstallProgress,"buildwrapper",IPreferenceConstants.BUILDWRAPPER_EXECUTABLE,Arrays.asList( cabalExecutable , "install","buildwrapper", global?"--global": "--user" )));
    }
    if (scionBrowser){
      commands.add(new Command(UITexts.scionBrowserInstallProgress,"scion-browser",IPreferenceConstants.SCION_BROWSER_SERVER_EXECUTABLE,Arrays.asList( cabalExecutable , "install","scion-browser", global?"--global": "--user" )));
    }*/
    for (Package p:packages){
      List<String> args=new ArrayList<>(Arrays.asList( cabalExecutable , "install",p.getPkgName() ));
      if (!CabalImplementationManager.getInstance().getDefaultCabalImplementation().allowsSandbox()){
        args.add(global?"--global": "--user");
      }
      args.add("--with-ghc="+CompilerManager.getCompilerExecutable());
      BackendManager.addCabalInstallOptions( args );
      File f=new File(binDir,FileUtil.makeExecutableName( p.getExeName() ));
      if (!f.exists()){ // the exe does not exist, we force reinstall to make sure it wasn't deleted manually
        args.add( "--reinstall" );
      }
      commands.add(new Command(NLS.bind( UITexts.installExecutableProgress,p.getExeName()),p.getExeName(),p.getPreference(),args));
    }
    final File fBinDir=binDir;
    final File ffolder=folder;
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
                AbstractHaskellLaunchDelegate.runInConsole( null, c.commands, ffolder, c.title, true,fNext );
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
    private String exeName;
    /**
     * the package name may be different than the exe name
     * example: yesod-bin is the package, installs the yesod exe
     */
    private String pkgName;
    private String preference;


    public Package( final String exeName, final String preference ) {
      this(exeName,exeName,preference);
    }

    public Package( final String exeName,final String pkgName, final String preference ) {
      super();
      this.exeName = exeName;
      this.pkgName = pkgName;
      this.preference = preference;
    }


    public String getExeName() {
      return exeName;
    }




    public void setExeName( final String exeName ) {
      this.exeName = exeName;
    }




    public String getPkgName() {
      return pkgName;
    }




    public void setPkgName( final String pkgName ) {
      this.pkgName = pkgName;
    }


    public String getPreference() {
      return preference;
    }

    public void setPreference( final String preference ) {
      this.preference = preference;
    }


  }




}
