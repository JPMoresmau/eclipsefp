/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.usage;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

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
	/**
	 * are we currently working?
	 */
	private AtomicBoolean working=new AtomicBoolean(false);
	
	public UsageThread() {
		super("UsageThread");
		setDaemon(true);
	}

	public void run(){
		/**
		 * create the API in a thread, since new UsageAPI does a new UsageDB, which loads the SQLite library, etc
		 * This can hang on MacOS, so let's no hand the UI thread
		 */
		final UsageAPI api=new UsageAPI();

		BuildWrapperPlugin.getDefault().setUsageAPI(api);
		while (!shouldStop){
			synchronized (ps) {
				try{ 
					ps.wait();
				} catch (InterruptedException ie){
					// noop
				}
			}
			if (!shouldStop){
				working.set(true);
				IProject p=getNext();
					
				while (p!=null && !shouldStop){
					boolean retAll=!api.knowsProject(p);
					BWFacade f=BuildWrapperPlugin.getFacade(p);
					if (f!=null){
						List<Component> cs=f.getComponents();
						for (Component c:cs){
							if (c.isBuildable()){
								f.generateUsage(c,retAll);
							}
						}
					}
					p=getNext();
				}
			}
			working.set(false);
		}
		
	}
	
	public boolean isWorking(){
		return working.get();
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
