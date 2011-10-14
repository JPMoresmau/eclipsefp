package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.BadPositionCategoryException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.Position;

/**
 * avoid concurrent modification exceptions on annotation position updates
 * @author JP Moresmau
 *
 */
public class SynchronizableDocument extends Document{

  @Override
  public synchronized Position[] getPositions( final String category )
      throws BadPositionCategoryException {
    return super.getPositions( category );
  }

  @Override
  public synchronized Position[] getPositions( final String category, final int offset, final int length,
      final boolean canStartBefore, final boolean canEndAfter )
      throws BadPositionCategoryException {
    return super.getPositions( category, offset, length, canStartBefore,
        canEndAfter );
  }

  @Override
  public synchronized void addPosition( final Position position ) throws BadLocationException {
    super.addPosition( position );
  }

  @Override
  public synchronized void addPosition( final String category, final Position position )
      throws BadLocationException, BadPositionCategoryException {
    super.addPosition( category, position );
  }

  @Override
  public synchronized void removePosition( final Position position ) {
    super.removePosition( position );
  }

  @Override
  public synchronized void removePosition( final String category, final Position position )
      throws BadPositionCategoryException {
    super.removePosition( category, position );
  }

}
