package net.sf.eclipsefp.haskell.scion.client.nls;

import org.eclipse.osgi.util.NLS;

public final class ScionText extends NLS {
  public static String sharedLexerScionInstance;
  public static String scion_console_title;
  public static String noproject;

  private static final String BUNDLE_NAME = ScionText.class.getPackage().getName() + ".sciontexts"; //$NON-NLS-1$

  static {
    NLS.initializeMessages( BUNDLE_NAME, ScionText.class );
  }
}
