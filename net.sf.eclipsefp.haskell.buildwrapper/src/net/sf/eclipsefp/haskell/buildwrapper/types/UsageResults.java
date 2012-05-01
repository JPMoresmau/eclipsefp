/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.types;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;

/**
 * @author JP Moresmau
 *
 */
public class UsageResults {
	private Map<IProject,Map<IFile,Collection<Location>>> allResults=new HashMap<IProject, Map<IFile,Collection<Location>>>();
	
	public void put(IFile file,Collection<Location> locs){
		IProject p=file.getProject();
		Map<IFile,Collection<Location>> m=allResults.get(p);
		if (m==null){
			m=new HashMap<IFile, Collection<Location>>();
			allResults.put(p, m);
		}
		Collection<Location> allLocs=m.get(file);
		if (allLocs==null){
			allLocs=new ArrayList<Location>();
			m.put(file, allLocs);
		}
		for (Location l:locs){
			l.setIFile(file);
		}
		allLocs.addAll(locs);
	}
	
	public Set<IProject> listProjects(){
		return allResults.keySet();
	}
	
	public Map<IFile,Collection<Location>> getUsageInProject(IProject p){
		return allResults.get(p);
	}
	
}
