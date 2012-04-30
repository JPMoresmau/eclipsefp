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
	private Map<IProject,Map<IFile,Collection<UsageLocation>>> allResults=new HashMap<IProject, Map<IFile,Collection<UsageLocation>>>();
	
	public void put(IFile file,Collection<UsageLocation> locs){
		IProject p=file.getProject();
		Map<IFile,Collection<UsageLocation>> m=allResults.get(p);
		if (m==null){
			m=new HashMap<IFile, Collection<UsageLocation>>();
			allResults.put(p, m);
		}
		Collection<UsageLocation> allLocs=m.get(file);
		if (allLocs==null){
			allLocs=new ArrayList<UsageResults.UsageLocation>();
			m.put(file, allLocs);
		}
		allLocs.addAll(locs);
	}
	
	public Set<IProject> listProjects(){
		return allResults.keySet();
	}
	
	public Map<IFile,Collection<UsageLocation>> getUsageInProject(IProject p){
		return allResults.get(p);
	}
	
	public static class UsageLocation {
		private int startLine;
		private int lengthInLine;
		
		public UsageLocation(int startLine, int lengthInLine) {
			super();
			this.startLine = startLine;
			this.lengthInLine = lengthInLine;
		}

		public UsageLocation(Location l) {
			super();
			this.startLine = l.getStartLine();
			this.lengthInLine = Math.max(1, l.getEndLine()-getStartLine());
		}
		
		/**
		 * @return the lengthInLine
		 */
		public int getLengthInLine() {
			return lengthInLine;
		}
		
		/**
		 * @return the startLine
		 */
		public int getStartLine() {
			return startLine;
		}
	}
}
