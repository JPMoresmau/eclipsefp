package net.sf.eclipsefp.test.util.common;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.Reader;
import java.net.URI;
import java.util.List;
import java.util.Vector;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFileState;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResourceProxy;
import org.eclipse.core.resources.IResourceProxyVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourceAttributes;
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

	public void appendContents(final InputStream source, final boolean force,
			final boolean keepHistory, final IProgressMonitor monitor) {
	  // unused
	}

	public void appendContents(final InputStream source, final int updateFlags,
			final IProgressMonitor monitor) {
	  // unused
	}

	public void create(final InputStream source, final boolean force,
			final IProgressMonitor monitor) {
	  // unused
	}

	public void create(final InputStream source, final int updateFlags,
			final IProgressMonitor monitor) {
	  // unused
	}

	public void createLink(final IPath localLocation, final int updateFlags,
			final IProgressMonitor monitor) {
	  // unused
	}

	public void delete(final boolean force, final boolean keepHistory,
			final IProgressMonitor monitor) {
	  // unused
	}

	public String getCharset() {
		return null;
	}

	public String getCharset(final boolean checkImplicit) {
		return null;
	}

	public String getCharsetFor(final Reader reader) {
		return null;
	}

	public IContentDescription getContentDescription() {
		return null;
	}

	public InputStream getContents() {
		MockInputStream stream = new MockInputStream(
				                     new ByteArrayInputStream(fContents.getBytes()));
		fOpenStreams.add(stream);
		return stream;
	}

	public InputStream getContents(final boolean force) {
		return getContents();
	}

	public int getEncoding() {
		return 0;
	}

	public IPath getFullPath() {
		return null;
	}

	public IFileState[] getHistory(final IProgressMonitor monitor) {
		return null;
	}

	public String getName() {
		return fFileName;
	}

	public boolean isReadOnly() {
		
		return false;
	}

	public void move(final IPath destination, final boolean force, final boolean keepHistory,
			final IProgressMonitor monitor) {
		// unused
	}

	public void setCharset(final String newCharset) {
	  // unused
	}

	public void setCharset(final String newCharset, final IProgressMonitor monitor) {
	  // unused
	}

	public void setContents(final InputStream source, final boolean force,
			final boolean keepHistory, final IProgressMonitor monitor) {
	  // unused
	}

	public void setContents(final IFileState source, final boolean force,
			final boolean keepHistory, final IProgressMonitor monitor) {
	  // unused
	}

	public void setContents(final InputStream source, final int updateFlags,
			final IProgressMonitor monitor) {
	  // unused
	}

	public void setContents(final IFileState source, final int updateFlags,
			final IProgressMonitor monitor) {
    // unused		
	}

	public void accept(final IResourceProxyVisitor visitor, final int memberFlags) {
	  // unused		
	}

	public void accept(final IResourceVisitor visitor) {
	  // unused		
	}

	public void accept(final IResourceVisitor visitor, final int depth,
			final boolean includePhantoms) {
	  // unused
	}

	public void accept(final IResourceVisitor visitor, final int depth, final int memberFlags) {
	  // unused		
	}

	public void clearHistory(final IProgressMonitor monitor) {
	  // unused
	}

	public void copy(final IPath destination, final boolean force, final IProgressMonitor monitor) {
	  // unused
	}

	public void copy(final IPath destination, final int updateFlags,
			final IProgressMonitor monitor) {
	  // unused
	}

	public void copy(final IProjectDescription description, final boolean force,
			final IProgressMonitor monitor) {
	  // unused
	}

	public void copy(final IProjectDescription description, final int updateFlags,
			final IProgressMonitor monitor) {
	  // unused
	}

	public IMarker createMarker(final String type) {
		return null;
	}

	public void delete(final boolean force, final IProgressMonitor monitor) {
	  // unused
	}

	public void delete(final int updateFlags, final IProgressMonitor monitor) {
	  // unused
	}

	public void deleteMarkers(final String type, final boolean includeSubtypes, final int depth) {
	  // unused
	}

	public boolean exists() {
		return false;
	}

	public IMarker findMarker(final long id) {
		
		return null;
	}

	public IMarker[] findMarkers(final String type, final boolean includeSubtypes, final int depth) {
		return null;
	}

	public String getFileExtension() {
		return null;
	}

	public long getLocalTimeStamp() {
		return 0;
	}

	public IPath getLocation() {
		return null;
	}

	public IMarker getMarker(final long id) {
		return null;
	}

	public long getModificationStamp() {
		return 0;
	}

	public IContainer getParent() {
		return null;
	}

	public String getPersistentProperty(final QualifiedName key) {
		return null;
	}

	public void setProject(final IProject project) {
		fProject = project;
	}

	public IProject getProject() {
		return fProject;
	}

	public IPath getProjectRelativePath() {
		return null;
	}

	public IPath getRawLocation() {
		return null;
	}

	public ResourceAttributes getResourceAttributes() {
		return null;
	}

	public Object getSessionProperty(final QualifiedName key) {
		return null;
	}

	public int getType() {
		
		return 0;
	}

	public IWorkspace getWorkspace() {
		
		return null;
	}

	public boolean isAccessible() {
		
		return false;
	}

	public boolean isDerived() {
		
		return false;
	}

	public boolean isLocal(final int depth) {
		
		return false;
	}

	public boolean isLinked() {
		
		return false;
	}

	public boolean isPhantom() {
		
		return false;
	}

	public boolean isSynchronized(final int depth) {
		
		return false;
	}

	public boolean isTeamPrivateMember() {
		
		return false;
	}

	public void move(final IPath destination, final boolean force, final IProgressMonitor monitor) {
    // unused
	}

	public void move(final IPath destination, final int updateFlags,
			final IProgressMonitor monitor) {
	  // unused
	}

	public void move(final IProjectDescription description, final boolean force,
			final boolean keepHistory, final IProgressMonitor monitor) {
	  // unused
	}

	public void move(final IProjectDescription description, final int updateFlags,
			final IProgressMonitor monitor) {
	  // unused
	}

	public void refreshLocal(final int depth, final IProgressMonitor monitor) {
	  // unused
	}

	public void revertModificationStamp(final long value) {
	  // unused
	}

	public void setDerived(final boolean isDerived) {
	  // unused
	}

	public void setLocal(final boolean flag, final int depth, final IProgressMonitor monitor) {
	  // unused
	}

	public long setLocalTimeStamp(final long value) {
		return 0;
	}

	public void setPersistentProperty(final QualifiedName key, final String value) {
	  // unused
	}

	public void setReadOnly(final boolean readOnly) {
	  // unused
	}

	public void setResourceAttributes(final ResourceAttributes attributes) {
		// unused
	}

	public void setSessionProperty(final QualifiedName key, final Object value) {
	  // unused
	}

	public void setTeamPrivateMember(final boolean isTeamPrivate) {
	  // unused
	}

	public void touch(final IProgressMonitor monitor) {
	  // unused
	}

	public Object getAdapter(final Class adapter) {
		return null;
	}

	public boolean contains(final ISchedulingRule rule) {
		
		return false;
	}

	public boolean isConflicting(final ISchedulingRule rule) {
		return false;
	}

	public void createLink(final URI location, final int updateFlags, final IProgressMonitor monitor) {
	  // unused
	}

	public URI getLocationURI() {
		return null;
	}

	public URI getRawLocationURI() {
		return null;
	}

	public boolean isLinked(final int options) {
		return false;
	}

	public void verify() {
		for (MockInputStream stream : fOpenStreams) {
			stream.verify();
		}
	}

	public IResourceProxy createProxy() {
		return null;
	}

	public int findMaxProblemSeverity(final String type, final boolean includeSubtypes,
			final int depth) {
		return 0;
	}

}
