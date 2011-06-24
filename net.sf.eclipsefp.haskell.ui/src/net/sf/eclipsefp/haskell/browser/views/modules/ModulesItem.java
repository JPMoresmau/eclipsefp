/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.modules;

import java.util.ArrayList;
import net.sf.eclipsefp.haskell.browser.items.Module;

public class ModulesItem {
	private final Object dbInfo;
	private final String name;
	private Module mod;
	private final ModulesItem parent;
	private final ArrayList<ModulesItem> children;

	public ModulesItem(final Object dbInfo, final String name, final Module mod) {
		this(dbInfo, name, mod, null);
	}

	public ModulesItem(final Object dbInfo, final String name, final Module mod, final ModulesItem parent) {
		this.dbInfo = dbInfo;
		this.name = name;
		this.mod = mod;
		this.parent = parent;
		this.children = new ArrayList<ModulesItem>();
	}

	public Object getDatabaseInfo() {
		return this.dbInfo;
	}

	public String getShownName() {
		return this.name;
	}

	public Module getModule() {
		return this.mod;
	}

	void setModule(final Module mod) {
		this.mod = mod;
	}

	public ModulesItem getParent() {
		return this.parent;
	}

	public void addChild(final ModulesItem child) {
		this.children.add(child);
	}

	ArrayList<ModulesItem> getModulesArrayList() {
		return this.children;
	}

	public ModulesItem[] getChildren() {
		return this.children.toArray(new ModulesItem[this.children.size()]);
	}

	public boolean hasChildren() {
		return !this.children.isEmpty();
	}
}
