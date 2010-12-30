package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.types.Location;

/**
 * The "token-at-point" scion command
 * 
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public class TokenAtPoint extends TokenRelativeToPoint {
  /** The usual constructor
   * 
   * @param contents The current document's contents
   * @param line The current line of the editor's point
   * @param column The current column of the editor's point
   * @param literate Literate Haskell source flag
   */
  public TokenAtPoint(String contents, Location editPoint, boolean literate) {
    super(contents, editPoint, literate);
  }

  @Override
  public String getMethod() {
    return "token-at-point";
  }
}
