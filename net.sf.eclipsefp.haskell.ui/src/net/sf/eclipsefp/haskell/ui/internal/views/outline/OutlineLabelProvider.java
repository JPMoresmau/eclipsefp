package net.sf.eclipsefp.haskell.ui.internal.views.outline;

import java.util.HashMap;
import java.util.Map;
import net.sf.eclipsefp.haskell.scion.types.OutlineDef;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

/**
 * <p>Display OutlineDef objects for the outline tree</p>
  *
  * @author JP Moresmau
 */
public class OutlineLabelProvider extends LabelProvider {
  private static Map<OutlineDef.OutlineDefType,String> imageKeysByType=new HashMap<OutlineDef.OutlineDefType, String>();

  static {
    imageKeysByType.put(OutlineDef.OutlineDefType.CLASS,IImageNames.CLASS_DECL);
    imageKeysByType.put(OutlineDef.OutlineDefType.DATA,IImageNames.DATA_DECL);
    imageKeysByType.put(OutlineDef.OutlineDefType.FUNCTION,IImageNames.FUNCTION_BINDING);
    imageKeysByType.put(OutlineDef.OutlineDefType.PATTERN,IImageNames.PATTERN_BINDING);
    imageKeysByType.put(OutlineDef.OutlineDefType.TYPE,IImageNames.TYPE_DECL);
    imageKeysByType.put(OutlineDef.OutlineDefType.SYN,IImageNames.TYPE_DECL);
  }

  @Override
  public String getText( final Object element ) {
    if (element instanceof OutlineDef){
      String s=((OutlineDef)element).getName();
      int ix=s.lastIndexOf( '.' );
      if (ix>-1){
        s=s.substring(ix+1);
      }
      return s;
    }
    return super.getText( element );
  }

  @Override
  public Image getImage( final Object element ) {
    if (element instanceof OutlineDef){
      String key=imageKeysByType.get( ((OutlineDef )element).getType());
      if (key!=null){
        return HaskellUIImages.getImage( key );
      }
    }
    return super.getImage( element );
  }

}
