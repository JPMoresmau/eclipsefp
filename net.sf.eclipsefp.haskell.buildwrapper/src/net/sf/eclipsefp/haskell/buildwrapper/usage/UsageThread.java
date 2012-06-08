/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.usage;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;

import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.Component;

import org.eclipse.core.resources.IProject;

/**
 * This thread calls the BuildWrapper executable and populates the database when needed
 * All requests to update a project are queued here
 * @author JP Moresmau
 *
 */
public class UsageThread extends Thread {
	private boolean shouldStop=false;
	
	private LinkedHashSet<IProject> ps=new LinkedHashSet<IProject>();
	
	public UsageThread() {
		super("UsageThread");
		setDaemon(true);
	}

	public void run(){
		while (!shouldStop){
			synchronized (ps) {
				try{ 
					ps.wait();
				} catch (InterruptedException ie){
					// noop
				}
			}
			if (!shouldStop){
				IProject p=getNext();
					
				while (p!=null && !shouldStop){
					boolean retAll=!BuildWrapperPlugin.getDefault().getUsageAPI().knowsProject(p);
					BWFacade f=BuildWrapperPlugin.getFacade(p);
					if (f!=null){
						List<Component> cs=f.getComponents();
						for (Component c:cs){
							if (c.isBuildable()){
								f.generateUsage(c,retAll);
							}
						}
						p=getNext();
					}
				}
			}
			
		}
		
	}
	
	private IProject getNext(){
		synchronized (ps) {
			if (!ps.isEmpty()){
				Iterator<IProject> it=ps.iterator();
				IProject p=it.next();
				it.remove();
				return p;
			}
		}
		return null;
	}
	
	public void addProject(IProject p){
		synchronized (ps) {
			ps.add(p);
			ps.notifyAll();
		}
	}
	
	/**
	 * @param shouldStop the shouldStop to set
	 */
	public void setShouldStop() {
		synchronized (ps) {
			this.shouldStop = true;
			ps.notifyAll();
		}
		
	}
}
