/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.usage;

import java.util.LinkedList;
import java.util.List;

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
	
	private LinkedList<IProject> ps=new LinkedList<IProject>();
	
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
				if (!shouldStop){
					IProject p=getNext();
						
					while (p!=null){
						boolean retAll=!BuildWrapperPlugin.getDefault().getUsageAPI().knowsProject(p);
						List<Component> cs=BuildWrapperPlugin.getFacade(p).getComponents();
						for (Component c:cs){
							BuildWrapperPlugin.getFacade(p).generateUsage(c,retAll);
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
				return ps.removeFirst();
			}
		}
		return null;
	}
	
	public void addProject(IProject p){
		synchronized (ps) {
			ps.addLast(p);
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
