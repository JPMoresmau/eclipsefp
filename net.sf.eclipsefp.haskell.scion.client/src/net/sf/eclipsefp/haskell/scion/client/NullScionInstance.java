package net.sf.eclipsefp.haskell.scion.client;

import java.io.File;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.jface.text.IDocument;

import net.sf.eclipsefp.haskell.scion.exceptions.ScionCommandException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.client.AbstractScionServer;
import net.sf.eclipsefp.haskell.scion.internal.client.IScionServer;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;
import net.sf.eclipsefp.haskell.scion.types.CabalPackage;
import net.sf.eclipsefp.haskell.scion.types.Component;
import net.sf.eclipsefp.haskell.scion.types.Location;
import net.sf.eclipsefp.haskell.scion.types.TokenDef;
import net.sf.eclipsefp.haskell.util.NullWriter;

public class NullScionInstance implements IScionInstance {
  private Writer                      serverOutput;
  private IProject                    project;
  private IFile                       loadedFile;

  private static class NullScionServerSingleton {
    private final static NullScionInstance theNullServer = new NullScionInstance();
  }

  public static NullScionInstance getDefaultInstance() {
    return NullScionServerSingleton.theNullServer;
  }
  
  private NullScionInstance() {
    this.serverOutput = new NullWriter();
  }
  
  public NullScionInstance (IPath serverExecutable, IProject project, Writer serverOutput) {
    this.project = project;
    this.serverOutput = serverOutput;
  }
  
  public void startServer() throws ScionServerStartupException {
    // Does nothing
  }

  public void stopServer() {
    // Does nothing
  }

  public void runCommandSync(ScionCommand command, IProgressMonitor monitor) throws ScionServerException, ScionCommandException {
    // Does nothing
  }

  public IProject getProject() {
    return project;
  }

  public boolean contains(ISchedulingRule rule) {
    return (   rule == this
            || (getProject() != null && rule == getProject())
            || (getProject() != null && (getProject().contains(rule)) ) );
  }

  public boolean isConflicting(ISchedulingRule rule) {
    return rule == this;
  }

  public void setServerExecutable(IPath serverExecutable) throws ScionServerStartupException {
    // NOP
  }

  public void start() throws ScionServerStartupException {
    // NOP
  }

  public void stop() {
    // NOP
  }

  public void buildProject(boolean output, boolean forceRecomp) {
    // NOP
  }

  public String thingAtPoint(Location location) {
    return null;
  }

  public Location firstDefinitionLocation(String name) {
    return null;
  }

  public List<TokenDef> tokenTypes(IFile file, String contents) {
    // Pass back an empty array:
    return new ArrayList<TokenDef>();
  }

  public void loadFile(IFile fileName, boolean sync) {
    loadedFile = fileName;
  }

  public void unloadFile(IFile fileName) {
    loadedFile = null;
  }

  public void reloadFile(IFile file, Runnable after, boolean sync) {
    // NOP
  }

  public void reloadFile(IFile file, IDocument doc, Runnable after, boolean sync) {
    // NOP
  }

  public void outline(IFile file, OutlineHandler handler, boolean sync) {
    // NOP
  }

  public boolean isLoaded(IFile f) {
    return loadedFile.equals(f);
  }

  public void definedNames(NameHandler handler) {
    // NOP
  }

  public void moduleGraph(NameHandler handler) {
    // NOP
  }

  public void listExposedModules(NameHandler handler) {
    // NOP
  }

  public Map<String, CabalPackage[]> getPackagesByDB() {
    return null;
  }

  public List<Component> getComponents() {
    return new ArrayList<Component>();
  }
}
