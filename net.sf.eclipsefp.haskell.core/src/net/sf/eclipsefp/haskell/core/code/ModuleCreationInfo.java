// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.code;

import java.util.Set;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;


/** <p>holds detailed information about the module which is to be created
  * in the 'New Module' wizard.</p>
  *
  * @author Leif Frenzel
  */
public class ModuleCreationInfo {

  private String moduleName = ""; //$NON-NLS-1$
  private IPath folders;
  private IContainer sourceContainer;
  private EHaskellCommentStyle fCommentStyle = EHaskellCommentStyle.USUAL;

  private Set<PackageDescriptionStanza> included;
  private Set<PackageDescriptionStanza> exposed;

  private PackageDescriptionStanza editorStanza;
  /**
   * are the source folders used to generate the qualified name
   */
  private boolean foldersQualify=true;

  /**
   * the name of the preference defining the module template
   */
  private String templatePreferenceName=ICorePreferenceNames.TEMPLATE_MODULE;

  private IProject project;

  // attribute setters and getters
  ////////////////////////////////

  public ModuleCreationInfo() {
    super();
  }

  public ModuleCreationInfo(final IFile f) {
    super();
    this.project=f.getProject();
    IContainer src=ResourceUtil.getSourceContainer( f );
    setSourceContainer( src );
    if (src!=null){
      IPath p=ResourceUtil.getSourceRelativePath( src, f );
      setFolders(p);
    }
    setModuleName( f.getProjectRelativePath().removeFileExtension().lastSegment() );

  }


  public IPath getFolders() {
    return folders;
  }

  public void setFolders( final IPath folders ) {
    this.folders = folders;
  }

  public String getModuleName() {
    return this.moduleName;
  }

  public String getQualifiedModuleName(){
    StringBuilder sb=new StringBuilder();
    if (foldersQualify && getFolders()!=null && getFolders().segmentCount()>0){
      String ps=getFolders().toPortableString().replace( '/', '.' ) ;
      sb.append(ps);
      if (!ps.endsWith( "." )){ //$NON-NLS-1$
          sb.append("."); //$NON-NLS-1$
      }
    }
    sb.append(getModuleName());
    return sb.toString();
  }

  public void setModuleName( final String moduleName ) {
    this.moduleName = moduleName;
  }

  public IContainer getSourceContainer() {
    return sourceContainer;
  }

  public void setSourceContainer( final IContainer sourceContainer ) {
    this.sourceContainer = sourceContainer;
    if (this.sourceContainer!=null){
      this.project=this.sourceContainer.getProject();
    }
  }

  public EHaskellCommentStyle getCommentStyle() {
    return fCommentStyle;
  }

  public void setCommentStyle( final EHaskellCommentStyle style ) {
    fCommentStyle = style;
  }


  public Set<PackageDescriptionStanza> getIncluded() {
    return included;
  }


  public void setIncluded( final Set<PackageDescriptionStanza> included ) {
    this.included = included;
  }


  public Set<PackageDescriptionStanza> getExposed() {
    return exposed;
  }


  public void setExposed( final Set<PackageDescriptionStanza> exposed ) {
    this.exposed = exposed;
  }


  public boolean isFoldersQualify() {
    return foldersQualify;
  }


  public void setFoldersQualify( final boolean foldersQualify ) {
    this.foldersQualify = foldersQualify;
  }


  public PackageDescriptionStanza getEditorStanza() {
    return editorStanza;
  }


  public void setEditorStanza( final PackageDescriptionStanza editorStanza ) {
    this.editorStanza = editorStanza;
  }


  public IProject getProject() {
    return project;
  }


  public void setProject( final IProject project ) {
    this.project = project;
  }


  public String getTemplatePreferenceName() {
    return templatePreferenceName;
  }


  public void setTemplatePreferenceName( final String templatePreferenceName ) {
    this.templatePreferenceName = templatePreferenceName;
  }


}