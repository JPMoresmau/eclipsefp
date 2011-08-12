package net.sf.eclipsefp.haskell.ui.wizards;

import java.io.IOException;
import java.io.InputStream;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;


public class NewUuagcPage extends WizardNewFileCreationPage {

  static final String FILE_NAME = "/newFiles/uuagc.ag";

  public NewUuagcPage( final IStructuredSelection selection ) {
    super("NewUuagcPage", selection);
    setTitle(UITexts.new_uuagc);
    setDescription(UITexts.uuagc_newFile);
    setFileExtension("ag");
  }

  @Override
  protected String getNewFileLabel() {
    return UITexts.new_uuagc;
  }

  @Override
  protected InputStream getInitialContents() {
    try {
      return HaskellUIPlugin.getDefault().getBundle().getEntry(FILE_NAME).openStream();
    } catch( IOException ex ) {
      // This should never happen
      return null;
    }
  }
}
