/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.Choice;

/**
 * Information associated with the choice of a type of test-suite.
 * @author Alejandro Serrano
 *
 */
public class TestSuiteTypeChoice extends Choice<TestSuiteType> {

  @Override
  public TestSuiteType[] getValues() {
    return TestSuiteType.values();
  }

  @Override
  public boolean allowOther() {
    return false;
  }

  @Override
  public TestSuiteType fromCabalString( final String s ) {
    for( TestSuiteType l:TestSuiteType.values() ) {
      if( l.getCabalName().equals( s ) ) {
        return l;
      }
    }
    return null;
  }

  @Override
  public String toCabalString( final TestSuiteType o ) {
    return o.getCabalName();
  }

  @Override
  public TestSuiteType fromShownString( final String s ) {
    for( TestSuiteType l: TestSuiteType.values() ) {
      if( l.getShownName().equals( s ) ) {
        return l;
      }
    }
    return null;
  }

  @Override
  public String toShownString( final TestSuiteType o ) {
    return o.getShownName();
  }

}
