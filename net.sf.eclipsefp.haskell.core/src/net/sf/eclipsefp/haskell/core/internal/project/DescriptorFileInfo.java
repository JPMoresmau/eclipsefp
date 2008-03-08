// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.internal.project;

/** <p>Encapsulates information about and contents of the descriptor file
  * that is created in the project. FP Projects store informations like
  * path settings etc. in these descriptor files.</p>
  *
  * @author Leif Frenzel
  */
public class DescriptorFileInfo {

  private final String name;
  private final String content;

  public DescriptorFileInfo( final String name, final String content ) {
    this.name = name;
    this.content = content;
  }


  // getters
  //////////

  String getName() {
    return name;
  }

  String getContent() {
    return content;
  }
}