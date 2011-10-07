package net.sf.eclipsefp.haskell.core.cabal;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.LinkedList;
import java.util.List;
import net.sf.eclipsefp.haskell.util.PlatformUtil;

/**
 * helper wrapping cabal list and cabal info operations
 * @author JP Moresmau
 *
 */
public class CabalPackageHelper {
  private final String cabalPath;

  private List<CabalPackageRef> installed=null;
  private List<CabalPackageRef> all=null;


  public CabalPackageHelper( final String cabalPath ) {
    super();
    this.cabalPath = cabalPath;
  }


  public List<CabalPackageRef> getInstalled()throws IOException {
    if (installed==null){
      installed=list("--installed"); //$NON-NLS-1$
    }
    return installed;
  }

  public List<CabalPackageRef> getAll()throws IOException {
    if (all==null){
      all=list(""); //$NON-NLS-1$
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

  private List<CabalPackageRef> list(final String opt)throws IOException{
    List<CabalPackageRef> ret=new LinkedList<CabalPackageRef>();
    BufferedReader br=run(cabalPath,"list",opt,"--simple-output");  //$NON-NLS-1$//$NON-NLS-2$
    String line=br.readLine();
    CabalPackageRef last=null;
    while (line!=null){
      CabalPackageRef r=parseRef(line);
      if (last==null || !r.getName().equals( last.getName() )){
        if (last!=null){
          last.getVersions().trimToSize();
        }
        ret.add(r);
        last=r;
      } else {
        last.getVersions().addAll(r.getVersions());
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

  private BufferedReader run(final String... opts) throws IOException{
    ProcessBuilder pb=new ProcessBuilder();
    pb.redirectErrorStream(false);
    pb.command(opts);

    Process p=pb.start();
    return new BufferedReader(new InputStreamReader(p.getInputStream(),"UTF8")); //$NON-NLS-1$

  }



  public void setInstalled( final List<CabalPackageRef> installed ) {
    this.installed = installed;
  }



  public void setAll( final List<CabalPackageRef> all ) {
    this.all = all;
  }

}
