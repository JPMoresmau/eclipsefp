package net.sf.eclipsefp.haskell.core.halamo;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Hashtable;
import java.util.Map;

/**
 * A Haskell language model controls relationships between Haskell language
 * elements. It is project-specific and at this time dependencies between
 * projects aren't allowed.
 * 
 * @author Thiago Arrais
 * 
 * @see HaskellModelManager#getModelFor(org.eclipse.core.resources.IProject)
 */
public class HaskellLanguageModel implements IHaskellModel {

	private Map<String, IModule> fModules = new Hashtable<String, IModule>();

	public HaskellLanguageModel() {
		//no need to do anything
	}

	public void putModule(IModule module) {
		fModules.put(module.getName(), module);
	}

	public IModule getModule(String name) {
		return fModules.get(name);
	}

	public Collection<IModule> getModules() {
		return new ArrayList<IModule>(fModules.values());
	}

	public Scope getScopeFor(IModule module) {
		return new Scope(module, this);
	}

	public void removeModule(String name) {
		fModules.remove(name);
	}

}