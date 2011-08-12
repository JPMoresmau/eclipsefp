package net.sf.eclipsefp.haskell.ui.wizards;

import java.io.IOException;
import java.io.InputStream;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;


public class NewAlexPage extends WizardNewFileCreationPage {

  static final String FILE_NAME = "/newFiles/alex.x";

  public NewAlexPage( final IStructuredSelection selection ) {
    super("NewAlexPage", selection);
    setTitle(UITexts.new_alex);
    setDescription(UITexts.alex_newFile);
    setFileExtension("x");
  }

  @Override
  protected String getNewFileLabel() {
    return UITexts.new_alex;
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
