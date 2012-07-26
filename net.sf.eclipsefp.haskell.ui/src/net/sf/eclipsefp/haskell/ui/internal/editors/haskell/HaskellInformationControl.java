/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.AbstractInformationControl;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.util.Geometry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.ProgressEvent;
import org.eclipse.swt.browser.ProgressListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

/**
 * Control showing hover or autocomplete info
 * @author Alejandro Serrano & Martijn Schrage
 *
 */
public class HaskellInformationControl extends AbstractInformationControl {

  private static final int HOVER_WRAPWIDTH = 600; // bounds for the tooltip with type info and errors/warnings
  private static final int HOVER_MAXWIDTH = 1000;
  private static final int HOVER_MINHEIGHT = 36;
  private static final int HOVER_MAXHEIGHT = 700;
  private Browser doc;
  private boolean hasContents = false;

  public HaskellInformationControl( final Shell parent ) {
    super( parent, true );
    create();
  }

  @Override
  public void setInformation( final String content ) {
    // If we need more information than just a string, implement IInformationControlExtension2 and method setInput,
    // which will be called with the object returned by getHoverInfo2 in HaskellTextHover.
    // TODO: with the method above, encode the type of the content (type info vs. error/warning)
    //       and use a smaller width for errors/warnings.
    setDocumentation( content );
  }

  @Override
  protected void createContent( final Composite parent ) {
    doc = new Browser( parent, SWT.TOP | SWT.RESIZE );

    // These have no effect, so we set the colors with css.
    // Unfortunately, the background remains white, which is sometimes visible when scrolling
    doc.setForeground( parent.getDisplay().getSystemColor( SWT.COLOR_INFO_FOREGROUND ) );
    doc.setBackground( parent.getDisplay().getSystemColor( SWT.COLOR_YELLOW ) );
    doc.setFont( JFaceResources.getTextFont() );

    // The only way to get the height of the browser to depend on the height of its
    // rendered contents seems to be through javascript:
    //   http://www.eclipse.org/forums/index.php/t/257935/
    doc.addProgressListener(new ProgressListener() {
      @Override
      public void completed(final ProgressEvent event) {
        // wait for page to load before evaluating any javascript

        int contentHeight = ((Double)doc.evaluate("return document.body.scrollHeight")).intValue();
        int contentWidth = ((Double)doc.evaluate("return document.body.scrollWidth")).intValue();

        // contentWidth may exceed HOVER_WRAPWIDTH if there are wide lines that cannot
        // be broken.

        if (contentWidth <= HOVER_WRAPWIDTH) {
          // for content within the width, simply set the content height and use
          // HOVER_WRAPWIDTH for the width. (cannot use contentWidth, since it sometimes
          // assumes an unnecessary vertical scrollbar and gets too small)
          HaskellInformationControl.this.setSize(HOVER_WRAPWIDTH,contentHeight);
        } else {
          // for content exceeding the width, we use the wider size and compute
          // a new content height.

          HaskellInformationControl.this.setSize(contentWidth,0);
          contentHeight = ((Double)doc.evaluate("return document.body.scrollHeight")).intValue();
          HaskellInformationControl.this.setSize(contentWidth,contentHeight);
        }
        // Unfortunately, once the width is set, it seems impossible to obtain
        // the actual rendered width. Therefore, the tooltip always has at least a
        // width of HOVER_WRAPWIDTH, even if the contents are not wrapped and less wide.
      }

      @Override
      public void changed(final ProgressEvent event) {
        // no need for changed events since content is only set once
      }
    });
  }

  /*
   * @see IInformationControl#setSize(int, int)
   */
  @Override
  public void setSize(final int width, final int height) {
    // Clip according to max width and min and max height. (min width is not necessary)
    super.setSize(Math.min(width,HOVER_MAXWIDTH), Math.max(HOVER_MINHEIGHT, Math.min(height,HOVER_MAXHEIGHT)));
  }

  public void setDocumentation( final String content ) {
    hasContents = content.length() > 0;
    doc.setText( "<html><body style=\"background-color: #fafbc5; margin:0; padding:0; font-size: 8pt\">"+content+"</body></html>" );
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
 /*
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

For some reason, the method above causes a very large tooltip to be generated, which is
briefly visible before it shrinks to its normal size. Unfortunately, this doesn't happen
on test runs, but only when the plug-in is deployed, making it extremely difficult to debug.
The problem doesn't seem to occur in Eclipse Juno.

For some other reason, everything works fine if we specify the size with computeSizeConstraints.
*/
  @Override
  public Point computeSizeConstraints(final int widthInChars, final int heightInChars) {
    return new Point(HOVER_WRAPWIDTH,0); // final height is set by ProgressListener when page is loaded
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
  @Override
  public boolean hasContents() {
    return this.hasContents;
  }

  @Override
  public void setFocus() {
    // super.setFocus();
    doc.setFocus();
  }

  // Without this, the hover text disappears immediately on a mouse move.
  /*
   * @see org.eclipse.jface.text.IInformationControlExtension5#getInformationPresenterControlCreator()
   * @since 3.4
   */
  @Override
  public IInformationControlCreator getInformationPresenterControlCreator() {
    return new IInformationControlCreator() {
      /*
       * @see org.eclipse.jface.text.IInformationControlCreator#createInformationControl(org.eclipse.swt.widgets.Shell)
       */
      @Override
      public IInformationControl createInformationControl(final Shell parent) {
        return new HaskellInformationControl(parent);
      }
    };
  }

}
