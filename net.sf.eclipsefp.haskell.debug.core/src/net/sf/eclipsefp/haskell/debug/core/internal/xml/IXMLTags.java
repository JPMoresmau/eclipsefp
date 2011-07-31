package net.sf.eclipsefp.haskell.debug.core.internal.xml;


public interface IXMLTags {
  static final String ELEMENT_TESTSUITE = "testsuite"; //$NON-NLS-1$
  static final String ELEMENT_TESTCASE = "testcase"; //$NON-NLS-1$
  static final String ELEMENT_FAILURE = "failure"; //$NON-NLS-1$

  static final String ATTRIB_NAME = "name"; //$NON-NLS-1$
  static final String DEFAULT_NAME = ""; //$NON-NLS-1$
  static final String ATTRIB_TIME = "time"; //$NON-NLS-1$
  static final String DEFAULT_TIME = "0"; //$NON-NLS-1$
}
