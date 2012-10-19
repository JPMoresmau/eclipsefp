package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import java.io.File;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.preference.StringButtonFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;

/**
 * A specialized {@link StringFieldEditor} that only allows executable files to be entered.
 * On Unix systems, this means that the file must be executable for the current user.
 * On Windows systems, it must have one of the common file extensions (.exe, .bat, ...).
 *
 * Ideally, this would inherit from {@link FileFieldEditor}, but that class was not written
 * with subclassing in mind. For example, it overrides {@link #checkState} with a version
 * that does not call {@link #doCheckState}, and it does not allow reading of its
 * <code>enforceAbsolute</code> setting.
 *
 * If the empty string is allowed, this is interpreted as being an optional file entry.
 * In that case, the file must only exist and be executable in case the field is not empty.
 *
 * @author thomas
 */
public class ExecutableFileFieldEditor extends StringButtonFieldEditor {

    /**
     * List of legal file extension suffixes, or <code>null</code>
     * for system defaults.
     */
    private String[] extensions = null;

    /**
     * Indicates whether the path must be absolute;
     * <code>false</code> by default.
     */
    private boolean enforceAbsolute = false;

    /**
     * Creates a new file field editor
     */
    protected ExecutableFileFieldEditor() {
      // nothing to do
    }

    /**
     * Creates a file field editor.
     *
     * @param name the name of the preference this field editor works on
     * @param labelText the label text of the field editor
     * @param parent the parent of the field editor's control
     */
    public ExecutableFileFieldEditor(final String name, final String labelText, final Composite parent) {
        this(name, labelText, false, parent);
    }

    /**
     * Creates a file field editor.
     *
     * @param name the name of the preference this field editor works on
     * @param labelText the label text of the field editor
     * @param enforceAbsolute <code>true</code> if the file path
     *  must be absolute, and <code>false</code> otherwise
     * @param parent the parent of the field editor's control
     */
    public ExecutableFileFieldEditor(final String name, final String labelText, final boolean enforceAbsolute, final Composite parent) {
        this(name, labelText, enforceAbsolute, VALIDATE_ON_FOCUS_LOST, parent);
    }

    /**
     * Creates a file field editor.
     *
     * @param name the name of the preference this field editor works on
     * @param labelText the label text of the field editor
     * @param enforceAbsolute <code>true</code> if the file path
     *  must be absolute, and <code>false</code> otherwise
     * @param validationStrategy either {@link StringButtonFieldEditor#VALIDATE_ON_KEY_STROKE}
     *  to perform on the fly checking, or {@link StringButtonFieldEditor#VALIDATE_ON_FOCUS_LOST}
     *  (the default) to perform validation only after the text has been typed in
     * @param parent the parent of the field editor's control.
     * @since 3.4
     * @see StringButtonFieldEditor#VALIDATE_ON_KEY_STROKE
     * @see StringButtonFieldEditor#VALIDATE_ON_FOCUS_LOST
     */
    public ExecutableFileFieldEditor(final String name, final String labelText,
            final boolean enforceAbsolute, final int validationStrategy, final Composite parent) {
        init(name, labelText);
        this.enforceAbsolute = enforceAbsolute;
        setErrorMessage(UITexts.executableFileFieldEditor_errorDoesNotExist);
        setChangeButtonText(UITexts.dots);
        setValidateStrategy(validationStrategy);
        createControl(parent);
    }

    /* (non-Javadoc)
    * @see org.eclipse.jface.preference.StringButtonFieldEditor#doFillIntoGrid(org.eclipse.swt.widgets.Composite, int)
    */
    @Override
    protected void doFillIntoGrid( final Composite parent, final int numColumns ) {

      super.doFillIntoGrid( parent, numColumns );
      Button b=getChangeControl( parent );
      b.setText( "" );
      b.setImage( HaskellUIImages.getImage( IImageNames.FOLDER )  );
      b.setToolTipText( JFaceResources.getString("openBrowse"));//$NON-NLS-1$
      b.setLayoutData( new GridData() );
    }

    /* (non-Javadoc)
     * Method declared on StringButtonFieldEditor.
     * Opens the file chooser dialog and returns the selected file.
     */
    @Override
	protected String changePressed() {
        File f = new File(getTextControl().getText());
        if (!f.exists()) {
			f = null;
		}
        File d = getFile(f);
        if (d == null) {
			return null;
		}

        return d.getAbsolutePath();
    }

    /**
     * Checks whether the text input field contains a valid value or not.
     *
     * @return <code>true</code> if the field value is valid,
     *   and <code>false</code> if invalid
     */
    @Override
    public boolean checkState() {
        if (getTextControl() == null) {
    			return false;
    		}
        String txt = getTextControl().getText();

        boolean result = isEmptyStringAllowed() || txt.length() > 0;
        result = result && doCheckState();

        if (result) {
    			clearErrorMessage();
    		} else {
    			showErrorMessage(getErrorMessage());
    		}

        return result;
    }

    /**
     * Helper to open the file chooser dialog.
     * @param startingDirectory the directory to open the dialog on.
     * @return File The File the user selected or <code>null</code> if they
     * do not.
     */
    private File getFile(final File startingDirectory) {

        FileDialog dialog = new FileDialog(getShell(), SWT.OPEN | SWT.SHEET);
        if (startingDirectory != null) {
			dialog.setFileName(startingDirectory.getPath());
		}
        if (extensions != null) {
			dialog.setFilterExtensions(extensions);
		}
        String file = dialog.open();
        if (file != null) {
            file = file.trim();
            if (file.length() > 0) {
				return new File(file);
			}
        }

        return null;
    }

    /**
     * Sets this file field editor's file extension filter.
     *
     * @param extensions a list of file extension, or <code>null</code>
     * to set the filter to the system's default value
     */
    public void setFileExtensions(final String[] extensions) {
        this.extensions = extensions;
    }

	/**
	 * Returns true if the current value is an existing file that is executable.
	 * If this cannot be determined with certainty, true is returned.
	 * If false is returned, the error message is set.
	 */
	@Override
	protected boolean doCheckState() {
		String fileName = getTextControl().getText();
		if (isEmptyStringAllowed() && fileName.length() == 0) {
      return true;
    }
		File file = new File(fileName);
		if (!file.exists() && !file.isAbsolute() && !enforceAbsolute){
		  String path = System.getenv("PATH"); //$NON-NLS-1$
	    for (String dir : path.split(File.pathSeparator)) {
	      file=new File(dir,fileName);
	      if (file.exists()){
	        break;
	      }
	    }
		}

		return checkFileExists(file) && checkFileAbsolute(file) && checkFileExecutable(file);
	}

	/**
	 * Checks whether the file exists and is a normal file.
	 * If not, returns false and sets the error message.
	 */
	protected boolean checkFileExists(final File file) {
        if (!file.isFile()) {
        	setErrorMessage( UITexts.executableFileFieldEditor_errorDoesNotExist );
            return false;
		}
		return true;
	}

	/**
	 * Checks wether the file path is absolute if that was requested.
	 * If not, returns false and sets the error message.
	 * Assumes that the file exists.
	 */
	protected boolean checkFileAbsolute(final File file) {
        if (enforceAbsolute && !file.isAbsolute()) {
			setErrorMessage( UITexts.executableFileFieldEditor_errorNotAbsolute );
			return false;
		}
        return true;
	}

	/**
	 * Checks whether the file is executable.
	 * If not, returns false and sets the error message.
	 * Assumes that the file exists.
	 */
	protected boolean checkFileExecutable(final File file) {
		if (!FileUtil.isExecutable(file)) {
			setErrorMessage(UITexts.executableFileFieldEditor_errorNotExecutable);
			return false;
		}
		return true;
	}

}
