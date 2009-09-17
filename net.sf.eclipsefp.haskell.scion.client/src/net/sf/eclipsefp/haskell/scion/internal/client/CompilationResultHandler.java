package net.sf.eclipsefp.haskell.scion.internal.client;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.internal.util.UITexts;
import net.sf.eclipsefp.haskell.scion.types.CompilationResult;
import net.sf.eclipsefp.haskell.scion.types.ICompilerResult;
import net.sf.eclipsefp.haskell.scion.types.Note;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;

public class CompilationResultHandler extends JobChangeAdapter {
	private IProject project;
	
	public CompilationResultHandler(IProject project) {
		super();
		this.project = project;
	}

	public void process(ICompilerResult r){
		CompilationResult cr=r.getCompilationResult();
		if (cr!=null){
			String root=project.getLocation().toOSString();
			for (Note n:cr.getNotes()){
				String s=n.getLocation().getFileName();
				if (s.startsWith(root)){
					s=s.substring(root.length());
				}
				IResource res=project.findMember(s);
				if (res!=null){
					try {
						n.applyAsMarker(res);
					}	catch( CoreException ex ) {
						ScionPlugin.logError(UITexts.error_applyMarkers, ex);
						ex.printStackTrace();
					}
				}
			}
		}
		if (r.hasOutput()){
			try {
				IResource res=project.findMember(ScionPlugin.DIST_FOLDER);
				if (res!=null){
					res.refreshLocal(IResource.DEPTH_INFINITE, null);
				} else {
					project.refreshLocal(IResource.DEPTH_INFINITE, null);
				}
			} catch (CoreException ce){
				
			}
		}
	}
	
	@Override
	public void done(IJobChangeEvent event) {
		
		if (event.getResult().isOK()) {
			ICompilerResult r=(ICompilerResult)event.getJob();
			process(r);
		}
	}
	
}
