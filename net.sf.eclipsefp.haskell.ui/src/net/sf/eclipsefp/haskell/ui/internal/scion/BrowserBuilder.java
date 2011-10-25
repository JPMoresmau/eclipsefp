// (c) 2010, B. Scott Michel (bscottm@ieee.org)
// Licensed under the EPL

package net.sf.eclipsefp.haskell.ui.internal.scion;

import java.io.InputStream;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;

/**
 * This class encapsulates the details of building the internal Browser server, such as
 * unpacking the internal zip archive to the staging directory and running cabal to
 * produce the scion-browser executable.
 *
 * @author B. Scott Michel (bscottm@ieee.org)
 */
@Deprecated
public class BrowserBuilder extends PackageBuilder {

  @Override
  public InputStream getArchive() {
    return BrowserPlugin.builinServerArchive();
  }

  @Override
  public String getMessage(final PackageBuilderMessages message) {
    switch(message) {
      case UNPACK_ARCHIVE_TITLE:
        return UITexts.unpackBrowserArchive_title;
      case ARCHIVE_RESOURCE_NOT_FOUND:
        return UITexts.browserArchiveResourceNotFound;
      case ARCHIVE_FILE_EXCEPTION:
        return UITexts.browserArchiveFileException;
      case ARCHIVE_NON_SPECIFIC_EXTENSION:
        return UITexts.browserArchiveNonspecificFileException;
      case BUILD_JOB_TITLE:
        return UITexts.browserBuildJob_title;
      case INSTALL_FAILED:
        return UITexts.browserServerInstallFailed;
      case INSTALL_ERROR:
        return UITexts.browserServerInstallError;
    }

    return null;
  }

  /** Does the built-in scion-server executable need building? (Note: this really only occurs if the
   * actual executable is not present in the staging directory.)
   */
  @Override
  public boolean needsBuilding() {
    return browserNeedsBuilding();
  }

  public static boolean browserNeedsBuilding() {
    return !BrowserPlugin.serverExecutablePath( BrowserPlugin.builtinServerDirectoryPath() ).toFile().exists();
  }

}
