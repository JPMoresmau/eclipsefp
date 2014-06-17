package net.sf.eclipsefp.haskell.core.project.util;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.Reader;
import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFileState;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResourceProxy;
import org.eclipse.core.resources.IResourceProxyVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourceAttributes;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.content.IContentDescription;
import org.eclipse.core.runtime.jobs.ISchedulingRule;

/**
 * A mocked IFile.
 *
 * Verifies if the underlying inputstream is closed.
 *
 * @author Thiago Arrais - thiago.arrais@gmailcom
 */
public class MockFile implements IFile {

	private final List<MockInputStream> fOpenStreams = new Vector<MockInputStream>();
	private final String fContents;
	private final String fFileName;
	private IProject fProject;

	public MockFile(final String contents) {
		this("Mock.hs", contents);
	}

	public MockFile(final String filename, final String contents) {
		fContents = contents;
		fFileName = filename;
	}

	@Override
  public void appendContents(final InputStream source, final boolean force,
			final boolean keepHistory, final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void appendContents(final InputStream source, final int updateFlags,
			final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void create(final InputStream source, final boolean force,
			final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void create(final InputStream source, final int updateFlags,
			final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void createLink(final IPath localLocation, final int updateFlags,
			final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void delete(final boolean force, final boolean keepHistory,
			final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public String getCharset() {
		return null;
	}

	@Override
  public String getCharset(final boolean checkImplicit) {
		return null;
	}

	@Override
  public String getCharsetFor(final Reader reader) {
		return null;
	}

	@Override
  public IContentDescription getContentDescription() {
		return null;
	}

	@Override
  public InputStream getContents() {
		MockInputStream stream = new MockInputStream(
				                     new ByteArrayInputStream(fContents.getBytes()));
		fOpenStreams.add(stream);
		return stream;
	}

	@Override
  public InputStream getContents(final boolean force) {
		return getContents();
	}

	@Override
  public boolean isDerived(final int options) {
		return false;
	}

	@Override
  public int getEncoding() {
		return 0;
	}

	@Override
  public IPath getFullPath() {
		return null;
	}

	@Override
  public boolean isHidden() {
		return false;
	}

	@Override
  public boolean isHidden(final int options) {
		return false;
	}

	@Override
  public IFileState[] getHistory(final IProgressMonitor monitor) {
		return null;
	}

	@Override
  public String getName() {
		return fFileName;
	}

	@Override
  public Map<QualifiedName, String> getPersistentProperties() throws CoreException {
		return null;
	}

	@Override
  public boolean isReadOnly() {
		return false;
	}

	@Override
  public Map<QualifiedName, Object> getSessionProperties() throws CoreException {
		return null;
	}

	@Override
  public boolean isTeamPrivateMember(final int options) {
		return false;
	}

	@Override
  public void move(final IPath destination, final boolean force, final boolean keepHistory,
			final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void setCharset(final String newCharset) {
	  // unused
	}

	@Override
  public void setCharset(final String newCharset, final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void setContents(final InputStream source, final boolean force,
			final boolean keepHistory, final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void setContents(final IFileState source, final boolean force,
			final boolean keepHistory, final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void setContents(final InputStream source, final int updateFlags,
			final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void setContents(final IFileState source, final int updateFlags,
			final IProgressMonitor monitor) {
      // unused
	}

	@Override
  public void setHidden(final boolean isHidden) throws CoreException {
	  // unused
	}

	@Override
  public void accept(final IResourceProxyVisitor visitor, final int memberFlags) {
	  // unused
	}

	@Override
  public void accept(final IResourceVisitor visitor) {
	  // unused
	}

	@Override
  public void accept(final IResourceVisitor visitor, final int depth,
			final boolean includePhantoms) {
	  // unused
	}

	@Override
  public void accept(final IResourceVisitor visitor, final int depth, final int memberFlags) {
	  // unused
	}

 @Override
public void accept(final IResourceProxyVisitor visitor, final int depth, final int memberFlags) {
	    // unused
  }

	@Override
  public void clearHistory(final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void copy(final IPath destination, final boolean force, final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void copy(final IPath destination, final int updateFlags,
			final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void copy(final IProjectDescription description, final boolean force,
			final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void copy(final IProjectDescription description, final int updateFlags,
			final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public IMarker createMarker(final String type) {
		return null;
	}

	@Override
  public void delete(final boolean force, final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void delete(final int updateFlags, final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void deleteMarkers(final String type, final boolean includeSubtypes, final int depth) {
	  // unused
	}

	@Override
  public boolean exists() {
		return false;
	}

	@Override
  public IMarker findMarker(final long id) {

		return null;
	}

	@Override
  public IMarker[] findMarkers(final String type, final boolean includeSubtypes, final int depth) {
		return null;
	}

	@Override
  public String getFileExtension() {
		return null;
	}

	@Override
  public long getLocalTimeStamp() {
		return 0;
	}

	@Override
  public IPath getLocation() {
		return null;
	}

	@Override
  public IMarker getMarker(final long id) {
		return null;
	}

	@Override
  public long getModificationStamp() {
		return 0;
	}

	@Override
  public IContainer getParent() {
		return null;
	}

	@Override
  public String getPersistentProperty(final QualifiedName key) {
		return null;
	}

	public void setProject(final IProject project) {
		fProject = project;
	}

	@Override
  public IProject getProject() {
		return fProject;
	}

	@Override
  public IPath getProjectRelativePath() {
		return null;
	}

	@Override
  public IPath getRawLocation() {
		return null;
	}

	@Override
  public ResourceAttributes getResourceAttributes() {
		return null;
	}

	@Override
  public Object getSessionProperty(final QualifiedName key) {
		return null;
	}

	@Override
  public int getType() {

		return 0;
	}

	@Override
  public IWorkspace getWorkspace() {

		return null;
	}

	@Override
  public boolean isAccessible() {

		return false;
	}

	@Override
  public boolean isDerived() {

		return false;
	}

	@Override
  public boolean isLocal(final int depth) {

		return false;
	}

	@Override
  public boolean isLinked() {

		return false;
	}

	@Override
  public boolean isPhantom() {

		return false;
	}

	@Override
  public boolean isSynchronized(final int depth) {

		return false;
	}

	@Override
  public boolean isTeamPrivateMember() {

		return false;
	}

	@Override
  public void move(final IPath destination, final boolean force, final IProgressMonitor monitor) {
    // unused
	}

	@Override
  public void move(final IPath destination, final int updateFlags,
			final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void move(final IProjectDescription description, final boolean force,
			final boolean keepHistory, final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void move(final IProjectDescription description, final int updateFlags,
			final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void refreshLocal(final int depth, final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public void revertModificationStamp(final long value) {
	  // unused
	}

	@Override
  public void setDerived(final boolean isDerived) {
	  // unused
	}

	@Override
  public void setLocal(final boolean flag, final int depth, final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public long setLocalTimeStamp(final long value) {
		return 0;
	}

	@Override
  public void setPersistentProperty(final QualifiedName key, final String value) {
	  // unused
	}

	@Override
  public void setReadOnly(final boolean readOnly) {
	  // unused
	}

	@Override
  public void setResourceAttributes(final ResourceAttributes attributes) {
		// unused
	}

	@Override
  public void setSessionProperty(final QualifiedName key, final Object value) {
	  // unused
	}

	@Override
  public void setTeamPrivateMember(final boolean isTeamPrivate) {
	  // unused
	}

	@Override
  public void touch(final IProgressMonitor monitor) {
	  // unused
	}

	public Object getAdapter(final Class adapter) {
		return null;
	}

	@Override
  public boolean contains(final ISchedulingRule rule) {

		return false;
	}

	@Override
  public boolean isConflicting(final ISchedulingRule rule) {
		return false;
	}

	@Override
  public void createLink(final URI location, final int updateFlags, final IProgressMonitor monitor) {
	  // unused
	}

	@Override
  public URI getLocationURI() {
		return null;
	}

	@Override
  public URI getRawLocationURI() {
		return null;
	}

	@Override
  public boolean isLinked(final int options) {
		return false;
	}

	public void verify() {
		for (MockInputStream stream : fOpenStreams) {
			stream.verify();
		}
	}

	@Override
  public IResourceProxy createProxy() {
		return null;
	}

	@Override
  public int findMaxProblemSeverity(final String type, final boolean includeSubtypes,
			final int depth) {
		return 0;
	}

  @Override
  public IPathVariableManager getPathVariableManager() {
    IPathVariableManager result = null;
    IWorkspace workspace = ResourcesPlugin.getWorkspace();
    if (workspace != null) {
      result = workspace.getPathVariableManager();
    }
    return result;
  }

  @Override
  public boolean isVirtual() {
    // By default, do not assume that MockFile's are virtual resources
    return false;
  }

  @Override
  public void setDerived( final boolean isDerived, final IProgressMonitor monitor )
      throws CoreException {
    // unused.
  }

}
