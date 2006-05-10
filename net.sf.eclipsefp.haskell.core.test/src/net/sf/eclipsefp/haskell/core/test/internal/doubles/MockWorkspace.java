package net.sf.eclipsefp.haskell.core.test.internal.doubles;

import java.io.InputStream;
import java.net.URI;
import java.util.Map;

import junit.framework.Assert;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNatureDescriptor;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceRuleFactory;
import org.eclipse.core.resources.ISaveParticipant;
import org.eclipse.core.resources.ISavedState;
import org.eclipse.core.resources.ISynchronizer;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceDescription;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.resources.WorkspaceLock;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.jobs.ISchedulingRule;

public class MockWorkspace implements IWorkspace {

	private IWorkspace fDelegateWorspace = ResourcesPlugin.getWorkspace();
	
	private int fTimesAddListenerCalled = 0;

	public void verify() {
		Assert.assertEquals(1, fTimesAddListenerCalled);
	}
	
	public void addResourceChangeListener(IResourceChangeListener listener) {
		fTimesAddListenerCalled++;
		fDelegateWorspace.addResourceChangeListener(listener);
	}

	public void addResourceChangeListener(IResourceChangeListener listener, int eventMask) {
		fTimesAddListenerCalled++;
		fDelegateWorspace.addResourceChangeListener(listener, eventMask);
	}

	public ISavedState addSaveParticipant(Plugin plugin, ISaveParticipant participant) throws CoreException {
		return fDelegateWorspace.addSaveParticipant(plugin, participant);
	}

	public void build(int kind, IProgressMonitor monitor) throws CoreException {
		fDelegateWorspace.build(kind, monitor);
	}

	public void checkpoint(boolean build) {
		fDelegateWorspace.checkpoint(build);
	}

	public IProject[][] computePrerequisiteOrder(IProject[] projects) {
		return fDelegateWorspace.computePrerequisiteOrder(projects);
	}

	public ProjectOrder computeProjectOrder(IProject[] projects) {
		return fDelegateWorspace.computeProjectOrder(projects);
	}

	public IStatus copy(IResource[] resources, IPath destination, boolean force, IProgressMonitor monitor) throws CoreException {
		return fDelegateWorspace.copy(resources, destination, force, monitor);
	}

	public IStatus copy(IResource[] resources, IPath destination, int updateFlags, IProgressMonitor monitor) throws CoreException {
		return fDelegateWorspace.copy(resources, destination, updateFlags, monitor);
	}

	public IStatus delete(IResource[] resources, boolean force, IProgressMonitor monitor) throws CoreException {
		return fDelegateWorspace.delete(resources, force, monitor);
	}

	public IStatus delete(IResource[] resources, int updateFlags, IProgressMonitor monitor) throws CoreException {
		return fDelegateWorspace.delete(resources, updateFlags, monitor);
	}

	public void deleteMarkers(IMarker[] markers) throws CoreException {
		fDelegateWorspace.deleteMarkers(markers);
	}

	public void forgetSavedTree(String pluginId) {
		fDelegateWorspace.forgetSavedTree(pluginId);
	}

	public Object getAdapter(Class adapter) {
		return fDelegateWorspace.getAdapter(adapter);
	}

	public Map getDanglingReferences() {
		return fDelegateWorspace.getDanglingReferences();
	}

	public IWorkspaceDescription getDescription() {
		return fDelegateWorspace.getDescription();
	}

	public IProjectNatureDescriptor getNatureDescriptor(String natureId) {
		return fDelegateWorspace.getNatureDescriptor(natureId);
	}

	public IProjectNatureDescriptor[] getNatureDescriptors() {
		return fDelegateWorspace.getNatureDescriptors();
	}

	public IPathVariableManager getPathVariableManager() {
		return fDelegateWorspace.getPathVariableManager();
	}

	public IWorkspaceRoot getRoot() {
		return fDelegateWorspace.getRoot();
	}

	public IResourceRuleFactory getRuleFactory() {
		return fDelegateWorspace.getRuleFactory();
	}

	public ISynchronizer getSynchronizer() {
		return fDelegateWorspace.getSynchronizer();
	}

	public boolean isAutoBuilding() {
		return fDelegateWorspace.isAutoBuilding();
	}

	public boolean isTreeLocked() {
		return fDelegateWorspace.isTreeLocked();
	}

	public IProjectDescription loadProjectDescription(InputStream projectDescriptionFile) throws CoreException {
		return fDelegateWorspace.loadProjectDescription(projectDescriptionFile);
	}

	public IProjectDescription loadProjectDescription(IPath projectDescriptionFile) throws CoreException {
		return fDelegateWorspace.loadProjectDescription(projectDescriptionFile);
	}

	public IStatus move(IResource[] resources, IPath destination, boolean force, IProgressMonitor monitor) throws CoreException {
		return fDelegateWorspace.move(resources, destination, force, monitor);
	}

	public IStatus move(IResource[] resources, IPath destination, int updateFlags, IProgressMonitor monitor) throws CoreException {
		return fDelegateWorspace.move(resources, destination, updateFlags, monitor);
	}

	public IProjectDescription newProjectDescription(String projectName) {
		return fDelegateWorspace.newProjectDescription(projectName);
	}

	public void removeResourceChangeListener(IResourceChangeListener listener) {
		fDelegateWorspace.removeResourceChangeListener(listener);
	}

	public void removeSaveParticipant(Plugin plugin) {
		fDelegateWorspace.removeSaveParticipant(plugin);
	}

	public void run(IWorkspaceRunnable action, IProgressMonitor monitor) throws CoreException {
		fDelegateWorspace.run(action, monitor);
	}

	public void run(IWorkspaceRunnable action, ISchedulingRule rule, int flags, IProgressMonitor monitor) throws CoreException {
		fDelegateWorspace.run(action, rule, flags, monitor);
	}

	public IStatus save(boolean full, IProgressMonitor monitor) throws CoreException {
		return fDelegateWorspace.save(full, monitor);
	}

	public void setDescription(IWorkspaceDescription description) throws CoreException {
		fDelegateWorspace.setDescription(description);
	}

	public void setWorkspaceLock(WorkspaceLock lock) {
		fDelegateWorspace.setWorkspaceLock(lock);
	}

	public String[] sortNatureSet(String[] natureIds) {
		return fDelegateWorspace.sortNatureSet(natureIds);
	}

	public IStatus validateEdit(IFile[] files, Object context) {
		return fDelegateWorspace.validateEdit(files, context);
	}

	public IStatus validateLinkLocation(IResource resource, IPath location) {
		return fDelegateWorspace.validateLinkLocation(resource, location);
	}

	public IStatus validateLinkLocationURI(IResource resource, URI location) {
		return fDelegateWorspace.validateLinkLocationURI(resource, location);
	}

	public IStatus validateName(String segment, int typeMask) {
		return fDelegateWorspace.validateName(segment, typeMask);
	}

	public IStatus validateNatureSet(String[] natureIds) {
		return fDelegateWorspace.validateNatureSet(natureIds);
	}

	public IStatus validatePath(String path, int typeMask) {
		return fDelegateWorspace.validatePath(path, typeMask);
	}

	public IStatus validateProjectLocation(IProject project, IPath location) {
		return fDelegateWorspace.validateProjectLocation(project, location);
	}

	public IStatus validateProjectLocationURI(IProject project, URI location) {
		return fDelegateWorspace.validateProjectLocationURI(project, location);
	}

}
