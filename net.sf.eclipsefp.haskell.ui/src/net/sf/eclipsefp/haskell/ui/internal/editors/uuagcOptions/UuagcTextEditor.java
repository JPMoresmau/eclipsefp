/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.uuagcOptions;

import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.editors.text.TextEditor;

/**
 * Text editor for uuagc_options file.
 * @author Alejandro Serrano
 *
 */
public class UuagcTextEditor extends TextEditor {

  public IDocument getDocument() {
    IDocument result = null;
    if( getSourceViewer() != null ) {
      result = getSourceViewer().getDocument();
    }
    return result;
  }
}
