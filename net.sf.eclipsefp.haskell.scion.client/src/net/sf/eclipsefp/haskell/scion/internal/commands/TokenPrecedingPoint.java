package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.types.Location;

/**
 * Get the token preceding the editor's point
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 *
 */
public class TokenPrecedingPoint extends TokenRelativeToPoint {
  /** The usual constructor
   * 
   * @param contents The current document's contents
   * @param line The current line of the editor's point
   * @param column The current column of the editor's point
   * @param literate Literate Haskell source flag
   */
  public TokenPrecedingPoint(String contents, Location editPoint, boolean literate) {
    super(contents, editPoint, literate);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getMethod() {
    return "token-preceding";
  }
}
