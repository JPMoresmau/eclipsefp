package net.sf.eclipsefp.haskell.browser.views.modules;

import java.util.ArrayList;

import net.sf.eclipsefp.haskell.browser.items.Module;

public class ModulesItem {
	private Object dbInfo;
	private String name;
	private Module mod;
	private ModulesItem parent;
	private ArrayList<ModulesItem> children;
	
	public ModulesItem(Object dbInfo, String name, Module mod) {
		this(dbInfo, name, mod, null);
	}
	
	public ModulesItem(Object dbInfo, String name, Module mod, ModulesItem parent) {
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
	
	void setModule(Module mod) {
		this.mod = mod;
	}
	
	public ModulesItem getParent() {
		return this.parent;
	}
	
	public void addChild(ModulesItem child) {
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
