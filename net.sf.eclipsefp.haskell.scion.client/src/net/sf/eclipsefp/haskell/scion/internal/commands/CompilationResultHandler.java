package net.sf.eclipsefp.haskell.scion.internal.commands;

import java.util.HashSet;
import java.util.Set;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.internal.util.UITexts;
import net.sf.eclipsefp.haskell.scion.types.CompilationResult;
import net.sf.eclipsefp.haskell.scion.types.ICompilerResult;
import net.sf.eclipsefp.haskell.scion.types.Note;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.jface.text.IDocument;

public class CompilationResultHandler extends JobChangeAdapter {
	private IProject project;
	private int maxLines=Integer.MAX_VALUE;
	private IPath documentPath;
	private Set<Note> processed=new HashSet<Note>();
	
	public CompilationResultHandler(IProject project) {
		super();
		this.project = project;
	}

	public CompilationResultHandler(IProject project,IDocument doc, IPath documentPath) {
		super();
		this.project = project;
		this.maxLines=doc.getNumberOfLines();
		this.documentPath=documentPath;
	}
	
	public void process(ICompilerResult r){
		CompilationResult cr=r.getCompilationResult();
		if (cr!=null){
			String root=project.getLocation().toOSString();
			for (final Note n:cr.getNotes()){
				if (processed.add(n)){
					String s=n.getLocation().getFileName();
					if (s.startsWith(root)){
						s=s.substring(root.length());
					}
					final IResource res=project.findMember(s);
					if (res!=null){
						int max=documentPath==null || !documentPath.equals(res.getLocation())?Integer.MAX_VALUE:maxLines;
						try {
							n.applyAsMarker(res,max);
						}	catch( CoreException ex ) {
							ScionPlugin.logError(UITexts.error_applyMarkers, ex);
							ex.printStackTrace();
						}
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
				ScionPlugin.logError(UITexts.error_refreshLocal, ce);
				ce.printStackTrace();
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
