package net.sf.eclipsefp.haskell.debug.ui.internal.debug;

import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import org.eclipse.core.resources.IFile;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.ILineBreakpoint;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.ui.IDebugModelPresentation;
import org.eclipse.debug.ui.IValueDetailListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.part.FileEditorInput;

/**
 * Presentation of Haskell debug elements in Haskell source editor
 * @author JP Moresmau
 *
 */
public class HaskellModelPresentation extends LabelProvider implements IDebugModelPresentation {

  public HaskellModelPresentation() {
    //NOOP
  }

  @Override
  public void computeDetail( final IValue value, final IValueDetailListener listener ) {
    String detail = ""; //$NON-NLS-1$
    try {
      detail = value.getValueString();
    } catch (DebugException e) {
      HaskellDebugCore.log( e.getLocalizedMessage(), e );
    }
    listener.detailComputed(value, detail);
  }

  @Override
  public Image getImage( final Object element ) {
    return null;
  }

  @Override
  public String getText( final Object element ) {
    return null;
  }

  @Override
  public void setAttribute( final String attribute, final Object value ) {
    // noop
  }


  @Override
  public String getEditorId( final IEditorInput input, final Object element ) {
    if (element instanceof IFile || element instanceof ILineBreakpoint) {
      return HaskellEditor.ID;
    }
    return null;
  }

  @Override
  public IEditorInput getEditorInput( final Object element ) {
    if (element instanceof IFile) {
      return new FileEditorInput((IFile)element);
    }
    if (element instanceof ILineBreakpoint) {
      return new FileEditorInput((IFile)((ILineBreakpoint)element).getMarker().getResource());
    }
    return null;
  }

}
