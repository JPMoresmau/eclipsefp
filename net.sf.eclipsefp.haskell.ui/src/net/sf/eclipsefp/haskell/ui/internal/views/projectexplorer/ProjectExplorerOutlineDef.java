package net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.scion.types.OutlineDef;
import net.sf.eclipsefp.haskell.ui.internal.views.outline.OutlineCP;
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
  /**
   * the same provider as outline view, to show children
   */
  private final OutlineCP treeContentProvider;

  public ProjectExplorerOutlineDef( final IFile owner, final OutlineDef outlineDef,final OutlineCP treeContentProvider) {
    super();
    this.owner = owner;
    this.outlineDef = outlineDef;
    this.treeContentProvider=treeContentProvider;
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
    Object[] def=treeContentProvider.getChildren( this.getOutlineDef() );
    List<ProjectExplorerOutlineDef> ret=new ArrayList<ProjectExplorerOutlineDef>( def.length );
    for (Object o:def){
      ret.add(new ProjectExplorerOutlineDef( getOwner(), (OutlineDef)o, treeContentProvider ));
    }
    return ret;
  }
}
