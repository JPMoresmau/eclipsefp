/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.core.cabal;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import net.sf.eclipsefp.haskell.util.ProcessRunner;
import org.eclipse.core.runtime.IStatus;

/**
 * helper wrapping cabal list and cabal info operations
 * @author JP Moresmau
 *
 */
public class CabalPackageHelper {
  private final String cabalPath;

  private List<CabalPackageRef> installed=null;
  private List<CabalPackageRef> all=null;

  /** singleton **/
  private static CabalPackageHelper instance;
  public static synchronized CabalPackageHelper getInstance(){
    if (instance==null || instance.getCabalPath()==null || !instance.getCabalPath().equals( CabalImplementationManager.getCabalExecutable() )){
      instance=new CabalPackageHelper( CabalImplementationManager.getCabalExecutable() );
    }
    return instance;
  }

  private CabalPackageHelper( final String cabalPath ) {
    super();
    this.cabalPath = cabalPath;
  }

  public void clear(){
    installed=null;
    all=null;
  }


  public String getCabalPath() {
    return cabalPath;
  }

  public boolean hasInstalledVersion(final String name,final String version) throws IOException{
    String s=getLastInstalledVersion(name);
    if (s!=null){
      return CabalPackageVersion.compare( s, version )>=0;
    }
    return false;
  }

  public String getLastInstalledVersion(final String name)throws IOException{
    List<CabalPackageRef> r=list(name,"--installed");//$NON-NLS-1$
    if (r.size()>0){
      return r.get( r.size()-1 ).toString();
    }
    return null;
  }

  public List<CabalPackageRef> getInstalled()throws IOException {
    if (installed==null){
      installed=list("","--installed"); //$NON-NLS-1$ //$NON-NLS-2$
      for (CabalPackageRef r:installed){
        r.getInstalled().addAll( r.getVersions() ); // all versions are installed
      }
    }
    return installed;
  }

  public List<CabalPackageRef> getAll()throws IOException {
    if (all==null){
      all=list("",""); //$NON-NLS-1$ //$NON-NLS-2$
      // check which versions are installed
      List<CabalPackageRef> installed=getInstalled();
      Map<String,CabalPackageRef> pkgByName=new HashMap<String, CabalPackageRef>();
      for (CabalPackageRef r:installed){
        pkgByName.put( r.getName(),r );
      }
      for (CabalPackageRef r:all){
        CabalPackageRef inst=pkgByName.get( r.getName() );
        if (inst!=null){
          r.getInstalled().addAll( inst.getInstalled() );
        }
      }
    }
    return all;
  }

  public String getInfo(final String name)throws IOException {
    BufferedReader br=run(cabalPath,"info",name);  //$NON-NLS-1$
    StringBuilder sb=new StringBuilder();
    String line=br.readLine();

    while (line!=null){
      sb.append(line);
      sb.append( PlatformUtil.NL );
      line=br.readLine();
    }
    br.close();
    return sb.toString().substring( 2 ); // starts with *<space>
  }

  private List<CabalPackageRef> list(final String pkg,final String opt)throws IOException{

    List<CabalPackageRef> ret=new LinkedList<CabalPackageRef>();
    if (cabalPath==null){
      return ret;
    }
    BufferedReader br=run(cabalPath,"list",pkg,opt,"--simple-output");  //$NON-NLS-1$//$NON-NLS-2$
    String line=br.readLine();
    CabalPackageRef last=null;
    while (line!=null){
      CabalPackageRef r=parseRef(line);
      if (r!=null){
        if (last==null || !r.getName().equals( last.getName() )){
          if (last!=null){
            last.getVersions().trimToSize();
          }
          ret.add(r);
          last=r;
        } else {
          last.getVersions().addAll(r.getVersions());
        }
      }
      line=br.readLine();
    }
    br.close();
    return ret;
  }

  private CabalPackageRef parseRef(final String line){
    if (line!=null && line.length()>0){
      int ix=line.indexOf( ' ' );
      String name=line.substring( 0,ix );
      String version=line.substring( ix+1 ).trim();
      CabalPackageRef r=new CabalPackageRef();
      r.setName(name);
      r.getVersions().add( version );
      return r;
    }
    return null;
  }

  /**
   * run the command and log any errors, returning a reader to the output
   * @param opts
   * @return
   * @throws IOException
   */
  private BufferedReader run(final String... opts) throws IOException{
    ProcessRunner pr=new ProcessRunner();
    StringWriter swOut=new StringWriter();
    StringWriter swErr=new StringWriter();
    pr.executeBlocking( new File("."), swOut, swErr, opts ); //$NON-NLS-1$
    String err=swErr.toString();
    if (err.length()>0){
      String warn="warning:";//$NON-NLS-1$
      // clearly, a warning
      if (err.toLowerCase().startsWith( warn )){
        HaskellCorePlugin.log( err.substring( warn.length() ).trim(), IStatus.WARNING );
      } else {
        HaskellCorePlugin.log( err, IStatus.ERROR );
      }
    }
    return new BufferedReader( new StringReader(swOut.toString()) );
  }



  public void setInstalled( final List<CabalPackageRef> installed ) {
    this.installed = installed;
  }



  public void setAll( final List<CabalPackageRef> all ) {
    this.all = all;
  }

}
