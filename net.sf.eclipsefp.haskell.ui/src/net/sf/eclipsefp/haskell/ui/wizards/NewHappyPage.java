package net.sf.eclipsefp.haskell.ui.wizards;

import java.io.IOException;
import java.io.InputStream;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;


public class NewHappyPage extends WizardNewFileCreationPage {

  static final String FILE_NAME = "/newFiles/happy.y";

  public NewHappyPage( final IStructuredSelection selection ) {
    super("NewHappyPage", selection);
    setTitle(UITexts.new_happy);
    setDescription(UITexts.happy_newFile);
    setFileExtension("y");
  }

  @Override
  protected String getNewFileLabel() {
    return UITexts.new_happy;
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
