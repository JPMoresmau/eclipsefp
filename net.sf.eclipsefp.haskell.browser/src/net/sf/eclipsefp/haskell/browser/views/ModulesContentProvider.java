package net.sf.eclipsefp.haskell.browser.views;

import java.util.ArrayList;

import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.items.Module;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

public class ModulesContentProvider implements ITreeContentProvider {

	boolean isHierarchical = false;

	ArrayList<ModulesItem> linearCache = null;
	ArrayList<ModulesItem> hierarchicalCache = null;

	public Object[] getElements(Object inputElement) {
		return isHierarchical ? hierarchicalCache.toArray() : linearCache.toArray();
	}

	public Object[] getChildren(Object parentElement) {
		if (!isHierarchical) {
			return new Object[0];
		} else {
			ModulesItem item = (ModulesItem) parentElement;
			return item.getChildren();
		}
	}

	public Object getParent(Object element) {
		ModulesItem item = (ModulesItem) element;
		return item.getParent();
	}

	public boolean hasChildren(Object element) {
		if (!isHierarchical) {
			return false;
		} else {
			ModulesItem item = (ModulesItem) element;
			return item.hasChildren();
		}
	}

	public void dispose() {
		// Do nothing
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		// Do nothing
		
		if (newInput == null) {
			linearCache = new ArrayList<ModulesItem>();
			hierarchicalCache = new ArrayList<ModulesItem>();
		} else {
			try {
				if (newInput instanceof DatabaseType) {
					BrowserPlugin.getSharedInstance().setCurrentDatabase((DatabaseType) newInput,
							null);
				} else {
					PackagesItem item = (PackagesItem) newInput;
					BrowserPlugin.getSharedInstance().setCurrentDatabase(DatabaseType.PACKAGE,
							item.getPackage().getIdentifier());
				}
				
				linearCache = new ArrayList<ModulesItem>();
				hierarchicalCache = new ArrayList<ModulesItem>();
				Module[] modules = BrowserPlugin.getSharedInstance().getAllModules();
				for (Module module : modules) {
					linearCache.add(new ModulesItem(newInput, module.getName(), module));
					addModuleToHierarchy(newInput, module);
				}
				
			} catch (Throwable ex) {
				linearCache = new ArrayList<ModulesItem>();
				hierarchicalCache = new ArrayList<ModulesItem>();
			}
		}
	}
	
	public void addModuleToHierarchy(Object dbInfo, Module m) {
		String[] names = m.getName().split(".");
		
		ArrayList<ModulesItem> currentList = hierarchicalCache;
		ModulesItem currentParent = null;
		for (int i = 0; i < names.length; i++) {
			ModulesItem currentItem = null;
			for (ModulesItem item : currentList) {
				if (item.getShownName() == names[i]) {
					currentItem = item;
					break;
				}
			}
			if (currentItem == null) {
				if (i == names.length - 1) // This is the last one
					currentItem = new ModulesItem(dbInfo, names[i], m, currentParent);
				else
					currentItem = new ModulesItem(dbInfo, names[i], null, currentParent);
				currentList.add(currentItem);
			}
			currentList = currentItem.getModulesArrayList();
			currentParent = currentItem;
		}
	}
}
