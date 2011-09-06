// (c) 2010, B. Scott Michel (bscottm@ieee.org)
// Licensed under the EPL

package net.sf.eclipsefp.haskell.ui.internal.scion;

import java.io.InputStream;
import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;

/**
 * This class encapsulates the details of building the internal Scion server, such as
 * unpacking the internal zip archive to the staging directory and running cabal to
 * produce the scion-server executable.
 *
 * @author B. Scott Michel (bscottm@ieee.org)
 */
public class ScionBuilder extends PackageBuilder {

  @Override
  public InputStream getArchive() {
    return ScionPlugin.builinServerArchive();
  }

  @Override
  public String getMessage(final PackageBuilderMessages message) {
    switch(message) {
      case UNPACK_ARCHIVE_TITLE:
        return UITexts.unpackScionArchive_title;
      case ARCHIVE_RESOURCE_NOT_FOUND:
        return UITexts.scionArchiveResourceNotFound;
      case ARCHIVE_FILE_EXCEPTION:
        return UITexts.scionArchiveFileException;
      case ARCHIVE_NON_SPECIFIC_EXTENSION:
        return UITexts.scionArchiveNonspecificFileException;
      case BUILD_JOB_TITLE:
        return UITexts.scionBuildJob_title;
      case INSTALL_FAILED:
        return UITexts.scionServerInstallFailed;
      case INSTALL_ERROR:
        return UITexts.scionServerInstallError;
    }

    return null;
  }

  /** Does the built-in scion-server executable need building? (Note: this really only occurs if the
   * actual executable is not present in the staging directory.)
   */
  @Override
  public boolean needsBuilding() {
    return scionNeedsBuilding();
  }

  public static boolean scionNeedsBuilding() {
    return !ScionPlugin.serverExecutablePath( ScionPlugin.builtinServerDirectoryPath() ).toFile().exists();
  }

}
