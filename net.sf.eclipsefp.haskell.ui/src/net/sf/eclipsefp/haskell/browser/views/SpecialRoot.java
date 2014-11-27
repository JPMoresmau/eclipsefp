package net.sf.eclipsefp.haskell.browser.views;

/**
 * Element to show when the database is not yet loaded.
 * @author Alejandro Serrano, JP Moresmau
 *
 */
public class SpecialRoot {
  public static SpecialRoot NO_DATABASE = new SpecialRoot();
  public static SpecialRoot EMPTY       = new SpecialRoot();
  public static SpecialRoot SEARCHING   = new SpecialRoot();
}
