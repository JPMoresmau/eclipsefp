package net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineDef;
import org.eclipse.core.resources.IFile;

/**
 * <p>Wrapper for OutlineDef objects in the project explorer: keep the IFile and the tree</p>
  *
  * @author JP Moresmau
 */
public class ProjectExplorerOutlineDef {
  /**
   * file owning the outlinedef
   */
  private final IFile owner;
  /**
   * the outline def itself
   */
  private final OutlineDef outlineDef;


  public ProjectExplorerOutlineDef( final IFile owner, final OutlineDef outlineDef) {
    super();
    this.owner = owner;
    this.outlineDef = outlineDef;
  }


  public OutlineDef getOutlineDef() {
    return outlineDef;
  }


  public IFile getOwner() {
    return owner;
  }

  /**
   * get the children from the tree content provider, wrapping them with the same data
   */
  public List<ProjectExplorerOutlineDef> getChildren(){
    List<OutlineDef> def=outlineDef.getChildren();
    List<ProjectExplorerOutlineDef> ret=new ArrayList<ProjectExplorerOutlineDef>( def.size() );
    for (Object o:def){
      ret.add(new ProjectExplorerOutlineDef( getOwner(), (OutlineDef)o ));
    }
    return ret;
  }
}
