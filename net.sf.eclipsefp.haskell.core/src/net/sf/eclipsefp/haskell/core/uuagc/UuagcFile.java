package net.sf.eclipsefp.haskell.core.uuagc;

import java.util.ArrayList;


public class UuagcFile {

  private String file;
  private final ArrayList<String> options;

  public UuagcFile(final String file) {
    this(file, new ArrayList<String>());
  }

  public UuagcFile(final String file, final ArrayList<String> options) {
    this.file = file;
    this.options = options;
  }

  public void rename(final String newName) {
    this.file = newName;
  }

  public boolean hasOption(final String option) {
    return options.contains( option );
  }

  public void addOption(final String option) {
    if (!hasOption( option )) {
      options.add( option );
    }
  }

  public void removeOption(final String option) {
    options.remove( option );
  }

  public String optionsString() {
    StringBuilder s = new StringBuilder();
    for (String option : options) {
      if (s.length() > 0) {
        s.append( ", " ); //$NON-NLS-1$
      }
      s.append( option );
    }
    return s.toString();
  }

  public String toUuagcString() {
    return "file : \"" + file + "\"\noptions : " + optionsString();  //$NON-NLS-1$ //$NON-NLS-2$
  }
}
