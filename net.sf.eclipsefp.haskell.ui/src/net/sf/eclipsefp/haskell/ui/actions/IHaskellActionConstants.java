package net.sf.eclipsefp.haskell.ui.actions;

/**
 * Action constants related to Haskell editor-specific actions.
  *
  * @author B. Scott Michel (bscottm@ieee.org
 */
public interface IHaskellActionConstants {
  /** The action prefix: This is the package name and needs to match the action identifiers in the Haskell UI's
   * plugin.xml. */
  static final String ActionPrefix = IHaskellActionConstants.class.getPackage().getName() + ".";

  /** Comment menu: name of Comment action */
  public static final String COMMENT = ActionPrefix.concat( "Comment" );

  /** Comment menu: name of Pragma Comment action */
  public static final String COMMENT_PRAGMA = ActionPrefix.concat( "Comment.Pragma" );

  /** Comment menu: name of Uncomment action */
  public static final String UNCOMMENT = ActionPrefix.concat( "Uncomment" );

  /** Source menu: name of standard Shift Right action */
  public static final String SHIFT_RIGHT = ActionPrefix.concat( "ShiftRight" ); //$NON-NLS-1$

  /** Source menu: name of standard Shift Left global action */
  public static final String SHIFT_LEFT = ActionPrefix.concat( "ShiftLeft" ); //$NON-NLS-1$

  /** Source -> Haddock submenu: name of the "document previous" action */
  public static final String HADDOCK_PREVIOUS = ActionPrefix.concat( "Haddock.Previous" );

  /** Source -> Haddock submenu: name of the "document following" action */
  public static final String HADDOCK_FOLLOWING = ActionPrefix.concat( "Haddock.Following" );

  /** Source -> Haddock submenu: name of the "document following" action */
  public static final String HADDOCK_BLOCK_FOLLOWING = ActionPrefix.concat( "Haddock.Block.Following" );

  /** Comment menu: name of Comment action */
  public static final String FORMAT = ActionPrefix.concat( "Format" );
}
