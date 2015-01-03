/**
 *  Copyright (c) 2014 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.types;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;

/**
 * Manages the buildwrapper long running processes
 * @author JP Moresmau
 *
 */
public class BWProcessManager {
	private Map<String,BWProcessInfo> buildProcesses=new HashMap<String,BWProcessInfo>();
	private Set<String> runningFiles=Collections.synchronizedSet(new HashSet<String>());
	
	/**
	 * the processes running, by thread, so we can kill them when cancelling
	 */
	private Map<Thread,Process> runningProcesses=Collections.synchronizedMap(new HashMap<Thread,Process>());
	
	
	/**
	 * 
	 */
	public BWProcessManager() {

	}

	public void startRunningFileWait(BWFileInfo bfi){
		String k=getKey(bfi);
		while (!runningFiles.add(k)){
			try {
				Thread.sleep(100);
			} catch (InterruptedException ignore){
				// noop
			}
		}
	}
	
	public boolean startRunningFile(BWFileInfo bfi){
		String k=getKey(bfi);
		return runningFiles.add(k);
	}
	
	public void stopRunningFile(BWFileInfo bfi){
		String k=getKey(bfi);
		runningFiles.remove(k);
	}
	
	private String getKey(BWFileInfo bfi){
		if (bfi.getStanza()!=null){
			return bfi.getStanza();
		}
		return bfi.getFile().getProjectRelativePath().toPortableString();
	}
	
	public synchronized BWProcessInfo getProcess(BWFileInfo bfi){
		return buildProcesses.get(getKey(bfi));
	}
	
	public synchronized void registerProcess(BWFileInfo bfi,Process p){
		String k=getKey(bfi);
		BWProcessInfo bpi=new BWProcessInfo(p, bfi.getFile());
		buildProcesses.put(k,bpi);
	}
	
	public synchronized boolean removeProcess(BWFileInfo bfi){
		String k=getKey(bfi);
		BWProcessInfo bpi=buildProcesses.get(k);
		if (bpi!=null){
			bpi.getFiles().remove(bfi.getFile());
			if (bpi.getFiles().isEmpty()){
				buildProcesses.remove(k);
				return true;
			}
		}
		return false;
	}
	
	public synchronized void removeProcessForce(BWFileInfo bfi){
		String k=getKey(bfi);
		buildProcesses.remove(k);
	}
	
	public synchronized boolean hasEnded(BWFileInfo bfi){
		String k=getKey(bfi);
		BWProcessInfo bpi=buildProcesses.get(k);
		if (bpi!=null){
			try {
				bpi.getProcess().exitValue();
				buildProcesses.remove(k);
				return true;
			} catch (IllegalThreadStateException itse){
				// noop
			}
		}
		return false;
	}
	
	
	public void registerProcess(Process p){
		runningProcesses.put(Thread.currentThread(), p);
	}
	
	public Process unregisterProcess(){
		return runningProcesses.remove(Thread.currentThread());
	}
	
	public Process unregisterProcess(Thread t){
		return runningProcesses.remove(t);
	}
	
	public synchronized void closeAll(){
		for (BWProcessInfo bpi:buildProcesses.values()){
			BWFacade.endProcess(bpi.getProcess());
		}
		buildProcesses.clear();
		
	}
}
