package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.AbstractInformationControl;
import org.eclipse.jface.util.Geometry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;


public class HaskellInformationControl extends AbstractInformationControl {

  private Browser doc;
  private boolean hasContents = false;

  public HaskellInformationControl( final Shell parent ) {
    super( parent, true );
    create();
  }

  @Override
  public void setInformation( final String content ) {
    setDocumentation( content );
  }

  @Override
  protected void createContent( final Composite parent ) {
    doc = new Browser( parent, SWT.NONE );
    doc.setForeground( parent.getForeground() );
    doc.setBackground( parent.getBackground() );
    doc.setFont( JFaceResources.getDialogFont() );
  }

  public void setDocumentation( final String content ) {
    hasContents = content.length() > 0;
    doc.setText( content );
  }

  /*
   * @see IInformationControl#setVisible(boolean)
   */
  @Override
  public void setVisible( final boolean visible ) {
    super.setVisible( visible );
    doc.setVisible( visible );
  }

  /*
   * @see IInformationControl#computeSizeHint()
   */
  @Override
  public Point computeSizeHint() {
    // see: https://bugs.eclipse.org/bugs/show_bug.cgi?id=117602
    int widthHint = SWT.DEFAULT;
    Point constraints = getSizeConstraints();
    if( constraints != null ) {
      widthHint = constraints.x;
    }
    return getShell().computeSize( widthHint, SWT.DEFAULT, true );
  }

  /*
   * @see org.eclipse.jface.text.AbstractInformationControl#computeTrim()
   */
  @Override
  public Rectangle computeTrim() {
    Rectangle r = doc.computeTrim( 0, 0, 0, 0 );
    return Geometry.add( super.computeTrim(), r );
  }

  /*
   * @see IInformationControl#setForegroundColor(Color)
   */
  @Override
  public void setForegroundColor( final Color foreground ) {
    super.setForegroundColor( foreground );
    doc.setForeground( foreground );
  }

  /*
   * @see IInformationControl#setBackgroundColor(Color)
   */
  @Override
  public void setBackgroundColor( final Color background ) {
    super.setBackgroundColor( background );
    doc.setBackground( background );
  }

  /*
   * @see IInformationControlExtension#hasContents()
   */
  public boolean hasContents() {
    return this.hasContents;
  }

  @Override
  public void setFocus() {
    // super.setFocus();
    doc.setFocus();
  }

}
