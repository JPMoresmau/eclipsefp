package net.sf.eclipsefp.haskell.scion.client;

import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.text.IDocument;

import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;
import net.sf.eclipsefp.haskell.scion.types.CabalPackage;
import net.sf.eclipsefp.haskell.scion.types.Component;
import net.sf.eclipsefp.haskell.scion.types.Location;
import net.sf.eclipsefp.haskell.scion.types.TokenDef;

public interface IScionInstance extends IScionCommandRunner {
  /** Set the server executable for this instance */
  public void setServerExecutable(final IPath serverExecutable) throws ScionServerStartupException;
  /** Start the actual server */
  public void start() throws ScionServerStartupException;
  /** Stop the actual server */
  public void stop();
  /** Build the project */
  public void buildProject(final boolean output, final boolean forceRecomp);
  public void outline(final IFile file, final OutlineHandler handler, final boolean sync);
  public String thingAtPoint(Location location);
  public Location firstDefinitionLocation(String name);
  public List<TokenDef> tokenTypes(final IFile file, final String contents);
  public void definedNames(final NameHandler handler);
  public void moduleGraph(final NameHandler handler);
  public void listExposedModules(final NameHandler handler);
  public Map<String, CabalPackage[]> getPackagesByDB();
  public List<Component> getComponents();
  
  public boolean isLoaded(IFile f);
  public void loadFile(IFile fileName, boolean sync);
  public void unloadFile(IFile fileName);
  public void reloadFile(final IFile file, final Runnable after, final boolean sync);
  public void reloadFile(final IFile file, final IDocument doc, final Runnable after, final boolean sync);
}