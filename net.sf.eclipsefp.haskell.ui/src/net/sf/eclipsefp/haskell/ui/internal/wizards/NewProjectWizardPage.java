/*******************************************************************************
 * Copyright (c) 2000, 2009 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package net.sf.eclipsefp.haskell.ui.internal.wizards;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.Observable;
import java.util.Observer;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.dialog.Validator;
import net.sf.eclipsefp.haskell.ui.dialog.ValidatorManager;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.DialogField;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.IDialogFieldListener;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.IStringButtonAdapter;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.LayoutUtil;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.SelectionButtonDialogField;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.StringButtonDialogField;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.StringDialogField;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.filesystem.URIUtil;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;

/**
 * Largely a copy of org.eclipse.jdt.ui.wizards.NewJavaProjectWizardPageOne.
 *
 * It would make sense to extend {@link WizardNewProjectCreationPage}. However,
 * that class does not allow setting the project name, so then we couldn't set
 * the project name based on the directory name of the existing source.
 *
 * @author Thomas ten Cate
 */
public class NewProjectWizardPage extends WizardPage {

  /**
   * Request a project name. Fires an event whenever the text field is
   * changed, regardless of its content.
   */
  private final class NameGroup extends Observable implements IDialogFieldListener {

    protected final StringDialogField fNameField;

    public NameGroup() {
      // text field for project name
      fNameField= new StringDialogField();
      fNameField.setLabelText(UITexts.newProjectWizardPage_NameGroup_label_text);
      fNameField.setDialogFieldListener(this);
    }

    public Control createControl(final Composite composite) {
      Composite nameComposite= new Composite(composite, SWT.NONE);
      nameComposite.setFont(composite.getFont());
      nameComposite.setLayout(initGridLayout(new GridLayout(2, false), false));

      fNameField.doFillIntoGrid(nameComposite, 2);
      LayoutUtil.setHorizontalGrabbing(fNameField.getTextControl(null));

      return nameComposite;
    }

    protected void fireEvent() {
      setChanged();
      notifyObservers();
    }

    public String getName() {
      return fNameField.getText().trim();
    }

    public void postSetFocus() {
      fNameField.postSetFocusOnDialogField(getShell().getDisplay());
    }

    public void setName(final String name) {
      fNameField.setText(name);
    }

    /* (non-Javadoc)
     * @see org.eclipse.jdt.internal.ui.wizards.dialogfields.IDialogFieldListener#dialogFieldChanged(org.eclipse.jdt.internal.ui.wizards.dialogfields.DialogField)
     */
    @Override
    public void dialogFieldChanged(final DialogField field) {
      fireEvent();
    }
  }

  /**
   * Request a location. Fires an event whenever the checkbox or the location
   * field is changed, regardless of whether the change originates from the
   * user or has been invoked programmatically.
   */
  private final class LocationGroup extends Observable implements Observer, IStringButtonAdapter, IDialogFieldListener {

    protected final SelectionButtonDialogField fWorkspaceRadio;
    protected final SelectionButtonDialogField fExternalRadio;
    protected final StringButtonDialogField fLocation;

    private String fPreviousExternalLocation;

    private final String DIALOGSTORE_LAST_EXTERNAL_LOC= HaskellUIPlugin.getPluginId() + ".last.external.project"; //$NON-NLS-1$

    public LocationGroup() {
      fWorkspaceRadio= new SelectionButtonDialogField(SWT.RADIO);
      fWorkspaceRadio.setDialogFieldListener(this);
      fWorkspaceRadio.setLabelText(UITexts.newProjectWizardPage_LocationGroup_workspace_desc);

      fExternalRadio= new SelectionButtonDialogField(SWT.RADIO);
      fExternalRadio.setLabelText(UITexts.newProjectWizardPage_LocationGroup_external_desc);

      fLocation= new StringButtonDialogField(this);
      fLocation.setDialogFieldListener(this);
      fLocation.setLabelText(UITexts.newProjectWizardPage_LocationGroup_locationLabel_desc);
      fLocation.setButtonLabel(UITexts.newProjectWizardPage_LocationGroup_browseButton_desc);

      fExternalRadio.attachDialogField(fLocation);

      fWorkspaceRadio.setSelection(true);
      fExternalRadio.setSelection(false);

      fPreviousExternalLocation= ""; //$NON-NLS-1$
    }

    public Control createControl(final Composite composite) {
      final int numColumns= 3;

      final Group group= new Group(composite, SWT.NONE);
      group.setLayout(initGridLayout(new GridLayout(numColumns, false), true));
      group.setText(UITexts.newProjectWizardPage_LocationGroup_title);

      fWorkspaceRadio.doFillIntoGrid(group, numColumns);
      fExternalRadio.doFillIntoGrid(group, numColumns);
      fLocation.doFillIntoGrid(group, numColumns);
      LayoutUtil.setHorizontalGrabbing(fLocation.getTextControl(null));

      return group;
    }

    protected void fireEvent() {
      setChanged();
      notifyObservers();
    }

    protected String getDefaultPath(final String name) {
      final IPath path= Platform.getLocation().append(name);
      return path.toOSString();
    }

    /* (non-Javadoc)
     * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
     */
    @Override
    public void update(final Observable o, final Object arg) {
      if (isWorkspaceRadioSelected()) {
        fLocation.setText(getDefaultPath(getProjectName()));
      }
      fireEvent();
    }

    public IPath getLocation() {
      if (isWorkspaceRadioSelected()) {
        return Platform.getLocation();
      }
      return Path.fromOSString(fLocation.getText().trim());
    }

    public boolean isWorkspaceRadioSelected() {
      return fWorkspaceRadio.isSelected();
    }

    /**
     * Returns <code>true</code> if the location is in the workspace
     *
     * @return <code>true</code> if the location is in the workspace
     */
    public boolean isLocationInWorkspace() {
      final String location= fLocationGroup.getLocation().toOSString();
      IPath projectPath= Path.fromOSString(location);
      return Platform.getLocation().isPrefixOf(projectPath);
    }


    public void setLocation(final IPath path) {
      fWorkspaceRadio.setSelection(path == null);
      if (path != null) {
        fLocation.setText(path.toOSString());
      } else {
        fLocation.setText(getDefaultPath(getProjectName()));
      }
      fireEvent();
    }

    /* (non-Javadoc)
     * @see org.eclipse.jdt.internal.ui.wizards.dialogfields.IStringButtonAdapter#changeControlPressed(org.eclipse.jdt.internal.ui.wizards.dialogfields.DialogField)
     */
    @Override
    public void changeControlPressed(final DialogField field) {
      final DirectoryDialog dialog= new DirectoryDialog(getShell());
      dialog.setMessage(UITexts.newProjectWizardPage_directory_message);
      String directoryName = fLocation.getText().trim();
      if (directoryName.length() == 0) {
        String prevLocation= HaskellUIPlugin.getDefault().getDialogSettings().get(DIALOGSTORE_LAST_EXTERNAL_LOC);
        if (prevLocation != null) {
          directoryName= prevLocation;
        }
      }

      if (directoryName.length() > 0) {
        final File path = new File(directoryName);
        if (path.exists()) {
          dialog.setFilterPath(directoryName);
        }
      }
      final String selectedDirectory = dialog.open();
      if (selectedDirectory != null) {
        String oldDirectory= new Path(fLocation.getText().trim()).lastSegment();
        fLocation.setText(selectedDirectory);
        String lastSegment= new Path(selectedDirectory).lastSegment();
        if (lastSegment != null && (getProjectName().length() == 0 || getProjectName().equals(oldDirectory))) {
          setProjectName(lastSegment);
        }
        HaskellUIPlugin.getDefault().getDialogSettings().put(DIALOGSTORE_LAST_EXTERNAL_LOC, selectedDirectory);
      }
    }

    /* (non-Javadoc)
     * @see org.eclipse.jdt.internal.ui.wizards.dialogfields.IDialogFieldListener#dialogFieldChanged(org.eclipse.jdt.internal.ui.wizards.dialogfields.DialogField)
     */
    @Override
    public void dialogFieldChanged(final DialogField field) {
      if (field == fWorkspaceRadio) {
        final boolean checked= fWorkspaceRadio.isSelected();
        if (checked) {
          fPreviousExternalLocation= fLocation.getText();
          fLocation.setText(getDefaultPath(fNameGroup.getName()));
        } else {
          fLocation.setText(fPreviousExternalLocation);
        }
      }
      fireEvent();
    }
  }

  private final class ComponentGroup extends Observable implements IDialogFieldListener{
    protected final SelectionButtonDialogField fExecutable;
    protected final SelectionButtonDialogField fLibrary;

    public ComponentGroup() {
      fExecutable= new SelectionButtonDialogField(SWT.CHECK);
      fExecutable.setDialogFieldListener(this);
      fExecutable.setLabelText(UITexts.newProjectWizardPage_ComponentGroup_executable);
      fExecutable.setSelection( true );

      fLibrary= new SelectionButtonDialogField(SWT.CHECK);
      fLibrary.setDialogFieldListener(this);
      fLibrary.setLabelText(UITexts.newProjectWizardPage_ComponentGroup_library);

    }

    public Control createControl(final Composite composite) {
      final int numColumns= 1;

      final Group group= new Group(composite, SWT.NONE);
      group.setLayout(initGridLayout(new GridLayout(numColumns, false), true));
      group.setText(UITexts.newProjectWizardPage_ComponentGroup_title);

      fExecutable.doFillIntoGrid(group, numColumns);
      fLibrary.doFillIntoGrid(group, numColumns);

      return group;
    }

    protected void fireEvent() {
      setChanged();
      notifyObservers();
    }

    @Override
    public void dialogFieldChanged( final DialogField field ) {
      fireEvent();

    }

    public boolean isLibrary(){
      return fLibrary.isSelected();
    }

    public boolean isExecutable(){
      return fExecutable.isSelected();
    }
  }

  /**
   * Validate this page and show appropriate warnings and error NewWizardMessages.
   */
  private final class PageValidator extends Validator {

    public PageValidator( final ValidatorManager manager ) {
      super( manager );
    }

    @Override
    protected void doUpdate() {

      final IWorkspace workspace= ResourcesPlugin.getWorkspace();

      final String name= fNameGroup.getName();

      // check whether the project name field is empty
      if (name.length() == 0) {
        setIncomplete(UITexts.newProjectWizardPage_Message_enterProjectName,IMessageProvider.INFORMATION);
        return;
      }

      // check whether the project name is valid
      final IStatus nameStatus= workspace.validateName(name, IResource.PROJECT);
      if (!nameStatus.isOK()) {
        setErrorMessage(nameStatus.getMessage());
        setPageComplete(false);
        return;
      }

      for (int a=0;a<name.length();a++){
        char c=name.charAt( a );
        if (!(Character.isDigit( c ) || Character.isLetter( c ) || c=='-')){
          setIncomplete(UITexts.newProjectWizardPage_Message_projectInvalidName);
          return;
        }
      }

      // check whether project already exists
      final IProject handle= workspace.getRoot().getProject(name);
      if (handle.exists()) {
        setIncomplete(UITexts.newProjectWizardPage_Message_projectAlreadyExists);
        return;
      }

      if (fLocationGroup.isWorkspaceRadioSelected()){
        IPath projectLocation= ResourcesPlugin.getWorkspace().getRoot().getLocation().append(name);
        if (projectLocation.toFile().exists()) {
          try {
            //correct casing
            String canonicalPath= projectLocation.toFile().getCanonicalPath();
            projectLocation= new Path(canonicalPath);
          } catch (IOException e) {
            HaskellUIPlugin.log(e);
          }

          String existingName= projectLocation.lastSegment();
          if (!existingName.equals(fNameGroup.getName())) {
            setIncomplete(NLS.bind(UITexts.newProjectWizardPageMessage_invalidProjectNameForWorkspaceRoot, existingName));
            return;
          }

        }
      }

      final String location= fLocationGroup.getLocation().toOSString();

      // check whether location is empty
      if (location.length() == 0) {
        setErrorMessage(null);
        setIncomplete(UITexts.newProjectWizardPage_Message_enterLocation);
        return;
      }

      // check whether the location is a syntactically correct path
      if (!Path.EMPTY.isValidPath(location)) {
        setIncomplete(UITexts.newProjectWizardPage_Message_invalidDirectory);
        return;
      }

      IPath projectPath= Path.fromOSString(location);

      if (fLocationGroup.isWorkspaceRadioSelected()) {
        projectPath= projectPath.append(fNameGroup.getName());
      }


      if (projectPath.toFile().exists()) {//create from existing source
        if (Platform.getLocation().isPrefixOf(projectPath)) { //create from existing source in workspace
          if (!Platform.getLocation().equals(projectPath.removeLastSegments(1))) {
            setErrorMessage(UITexts.newProjectWizardPage_Message_notOnWorkspaceRoot);
            setPageComplete(false);
            return;
          }

          if (!projectPath.toFile().exists()) {
            setIncomplete(UITexts.newProjectWizardPage_Message_notExistingProjectOnWorkspaceRoot);
            return;
          }
        }
       setWarningMessage( UITexts.newProjectWizardPage_Message_alreadyExists);
       setPageComplete(true);
       return;
      } else if (!fLocationGroup.isWorkspaceRadioSelected()) {//create at non existing external location
        if (!canCreate(projectPath.toFile())) {
          setIncomplete(UITexts.newProjectWizardPage_Message_cannotCreateAtExternalLocation);
          return;
        }

        // If we do not place the contents in the workspace validate the
        // location.
        final IStatus locationStatus= workspace.validateProjectLocation(handle, projectPath);
        if (!locationStatus.isOK()) {
          setIncomplete(locationStatus.getMessage());
          return;
        }
      }

      setPageComplete(true);
      setMessage(null);
    }

    private boolean canCreate(final File file) {
      File parent = file;
      while (!parent.exists()) {
        parent= parent.getParentFile();
        if (parent == null) {
          return false;
        }
      }

      return parent.canWrite();
    }
  }

  private static final String PAGE_NAME= "NewProjectWizardPage"; //$NON-NLS-1$

  private final ValidatorManager fValidatorManager;
  private PageValidator fValidator;
  private final NameGroup fNameGroup;
  private final LocationGroup fLocationGroup;
  private final ComponentGroup fComponentGroup;

  public NewProjectWizardPage() {
    this(PAGE_NAME);
  }

  public NewProjectWizardPage( final String pageName ) {
    super( pageName );
    fNameGroup= new NameGroup();
    fLocationGroup= new LocationGroup();
    fComponentGroup=new ComponentGroup();

    // establish connections
    fNameGroup.addObserver(fLocationGroup);
    // initialize all elements
    fNameGroup.notifyObservers();

    // create and connect validator
    fValidatorManager= new ValidatorManager(this);
    createValidators( fValidatorManager );
    fNameGroup.addObserver(fValidator);
    fLocationGroup.addObserver(fValidator);

    // initialize defaults
    setProjectName(""); //$NON-NLS-1$
    setProjectLocationURI(null);

  }

  /**
   * Subclasses may override to add additional validators,
   * but must call the superclass method.
   */
  protected void createValidators(final ValidatorManager manager) {
    fValidator = new PageValidator(manager);
  }

  protected ValidatorManager getValidatorManager() {
    return fValidatorManager;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   *
   * Subclasses should not override; rather, override {@link #createControls}.
   */
  @Override
  public void createControl(final Composite parent) {
    initializeDialogUnits(parent);

    final Composite composite= new Composite(parent, SWT.NULL);
    composite.setFont(parent.getFont());
    composite.setLayout(initGridLayout(new GridLayout(1, false), true));
    composite.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL));

    createControls(composite);

    setControl(composite);

    fValidatorManager.fullUpdate();
  }

  /**
   * Creates the controls within this page.
   * Subclasses may override.
   */
  protected void createControls(final Composite composite) {
    createNameControl(composite);
    createLocationControl(composite);
    createComponentControl(composite);
  }

  /**
   * Creates the controls for the name field.
   *
   * @param composite the parent composite
   * @return the created control
   */
  protected Control createNameControl(final Composite composite) {
    Control nameControl = fNameGroup.createControl(composite);
    nameControl.setLayoutData(horizontalFillGridData());
    return nameControl;
  }

  /**
   * Creates the controls for the location field.
   *
   * @param composite the parent composite
   * @return the created control
   */
  protected Control createLocationControl(final Composite composite) {
    Control locationControl = fLocationGroup.createControl(composite);
    locationControl.setLayoutData(horizontalFillGridData());
    return locationControl;
  }

  /**
   * Creates the controls for the name field.
   *
   * @param composite the parent composite
   * @return the created control
   */
  protected Control createComponentControl(final Composite composite) {
    Control componentControl = fComponentGroup.createControl(composite);
    componentControl.setLayoutData(horizontalFillGridData());
    return componentControl;
  }

  public boolean isLibrary(){
    return fComponentGroup.isLibrary();
  }

  public boolean isExecutable(){
    return fComponentGroup.isExecutable();
  }

  /**
   * Gets a project name for the new project.
   *
   * @return the new project resource handle
   */
  public String getProjectName() {
    return fNameGroup.getName();
  }

  /**
   * Sets the name of the new project
   *
   * @param name the new name
   */
  public void setProjectName(final String name) {
    if (name == null) {
      throw new IllegalArgumentException();
    }

    fNameGroup.setName(name);
  }

  /**
   * Returns the current project location path as entered by the user, or <code>null</code>
   * if the project should be created in the workspace.

   * @return the project location path or its anticipated initial value.
   */
  public URI getProjectLocationURI() {
    if (fLocationGroup.isLocationInWorkspace()) {
      return null;
    }
    return URIUtil.toURI(fLocationGroup.getLocation());
  }

  /**
   * Sets the project location of the new project or <code>null</code> if the project
   * should be created in the workspace
   *
   * @param uri the new project location
   */
  public void setProjectLocationURI(final URI uri) {
    IPath path= uri != null ? URIUtil.toPath(uri) : null;
    fLocationGroup.setLocation(path);
  }

  /**
   * Creates a project resource handle for the current project name field
   * value. The project handle is created relative to the workspace root.
   * <p>
   * This method does not create the project resource; this is the
   * responsibility of <code>IProject::create</code> invoked by the new
   * project resource wizard.
   * </p>
   *
   * @return the new project resource handle
   */
    public IProject getProjectHandle() {
        return ResourcesPlugin.getWorkspace().getRoot().getProject(
                getProjectName());
    }

    /**
     * Returns the current project location path as entered by
     * the user, or its anticipated initial value.
     * Note that if the default has been returned the path
     * in a project description used to create a project
     * should not be set.
     *
     * @return the project location path or its anticipated initial value.
     */
    public IPath getProjectLocationPath() {
      return fLocationGroup.getLocation();
    }

  /* (non-Javadoc)
   * @see org.eclipse.jface.dialogs.DialogPage#setVisible(boolean)
   */
  @Override
  public void setVisible(final boolean visible) {
    super.setVisible(visible);
    if (visible) {
      fNameGroup.postSetFocus();
    }
  }

  protected GridLayout initGridLayout(final GridLayout layout, final boolean margins) {
    layout.horizontalSpacing= convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_SPACING);
    layout.verticalSpacing= convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_SPACING);
    if (margins) {
      layout.marginWidth= convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_MARGIN);
      layout.marginHeight= convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_MARGIN);
    } else {
      layout.marginWidth= 0;
      layout.marginHeight= 0;
    }
    return layout;
  }

  protected GridData horizontalFillGridData() {
    return new GridData(GridData.FILL_HORIZONTAL);
  }

}
