package net.sf.eclipsefp.haskell.profiler.internal.editors;

import java.io.InputStream;
import java.math.BigInteger;
import java.util.Map;

import net.sf.eclipsefp.haskell.profiler.model.Job;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.EditorPart;

public class ProfilerViewer extends EditorPart {

	Job job = null;
	
	public ProfilerViewer() {
		super();
	}
	
	@Override
	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		try {
			IFileEditorInput fInput = (IFileEditorInput)input;
			InputStream contents = fInput.getFile().getContents();
			job = Job.parse(contents);
			contents.close();
			for(Map.Entry<String, BigInteger> entry : job.sortEntriesByTotal()) {
				System.out.print(entry.getKey());
				System.out.print(": ");
				System.out.println(entry.getValue().toString());
			}
		} catch (Exception e) {
			throw new PartInitException(Status.CANCEL_STATUS);
		}
	}
	
	@Override
	public void createPartControl(Composite parent) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setFocus() {
		// Do nothing
	}

	@Override
	public void doSave(IProgressMonitor monitor) {
		// Do nothing: the .hp files cannot be changed
	}

	@Override
	public void doSaveAs() {
		// Do nothing: the .hp files cannot be changed
	}

	@Override
	public boolean isDirty() {
		return false;
	}

	@Override
	public boolean isSaveAsAllowed() {
		return false;
	}



}
