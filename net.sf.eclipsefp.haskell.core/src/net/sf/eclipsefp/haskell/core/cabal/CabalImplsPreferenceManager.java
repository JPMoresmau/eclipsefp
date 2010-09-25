/**
 * Copyright (c) 2010, B. Scott Michel (scooter.phd@gmail.com)
 *
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.core.cabal;

import java.util.List;

/** Manage serializing and deserializing Cabal implementation preference
 * data and XML.
 *
 * @author B. Scott Michel
 *
 */
public class CabalImplsPreferenceManager {
  private static final String PREAMBLE = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"; //$NON-NLS-1$
  private static final String ELEM_TOP_LEVEL = "cabalImpls"; //$NON-NLS-1$
  private static final String ELEM_CABAL_IMPL = "implementation"; //$NON-NLS-1$

  private static final String ATT_NAME    = "name"; //$NON-NLS-1$
  private static final String ATT_EXECUTABLE = "executable"; //$NON-NLS-1$
  private static final String ATT_INSTALL_VERSION = "install-version"; //$NON-NLS-1$
  private static final String ATT_LIBRARY_VERSION = "library-version"; //$NON-NLS-1$

  public static String toXML( final List<CabalImplementation> impls ) {
    StringBuilder sb = new StringBuilder( PREAMBLE );
    sb.append( "<" ); //$NON-NLS-1$
    sb.append( ELEM_TOP_LEVEL );
    sb.append( ">\n" ); //$NON-NLS-1$
    for( CabalImplementation impl: impls ) {
      toXML( impl, sb );
    }
    sb.append( "</" ).append( ELEM_TOP_LEVEL ).append( ">\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    return sb.toString();
  }

  private static void toXML( final CabalImplementation impl, final StringBuilder sb) {
    sb.append("<")
      .append(ELEM_CABAL_IMPL)
      .append( appendAtt(ATT_NAME, impl.getUserIdentifier()) )
      .append( appendAtt( ATT_EXECUTABLE, impl.getCabalExecutableName().toPortableString() ) )
      .append( appendAtt( ATT_INSTALL_VERSION, impl.getInstallVersion() ) )
      .append( appendAtt( ATT_LIBRARY_VERSION, impl.getLibraryVersion() ) )
      .append( "/>" ); //$NON-NLS-1$
  }

  private static StringBuilder appendAtt( final String att, final String value ) {
    StringBuilder sb = new StringBuilder();

    sb.append( " " ) //$NON-NLS-1$
      .append( att )
      .append( "=\"" ) //$NON-NLS-1$
      .append( value )
      .append( "\"" ); //$NON-NLS-1$

    return sb;
  }
}