package net.sf.eclipsefp.haskell.browser.views.modules;

import java.util.ArrayList;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.items.Module;
import net.sf.eclipsefp.haskell.browser.views.packages.PackagesItem;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.IMemento;

public class ModulesContentProvider implements ITreeContentProvider {

	boolean isHierarchical;
	static final String MEMENTO_KEY = "isHierarchical";

	ArrayList<ModulesItem> linearCache = null;
	ArrayList<ModulesItem> hierarchicalCache = null;

	public ModulesContentProvider(final IMemento memento) {
		super();
		if (memento == null) {
      isHierarchical = false;
    } else {
			Boolean value = memento.getBoolean(MEMENTO_KEY);
			if (value == null) {
        isHierarchical = false;
      } else {
        isHierarchical = value;
      }
		}
	}

	public void saveState(final IMemento memento) {
		memento.putBoolean(MEMENTO_KEY, isHierarchical);
	}

	public Object[] getElements(final Object inputElement) {
		return isHierarchical ? hierarchicalCache.toArray() : linearCache.toArray();
	}

	public Object[] getChildren(final Object parentElement) {
		if (!isHierarchical) {
			return new Object[0];
		} else {
			ModulesItem item = (ModulesItem) parentElement;
			return item.getChildren();
		}
	}

	public Object getParent(final Object element) {
		ModulesItem item = (ModulesItem) element;
		return item.getParent();
	}

	public boolean hasChildren(final Object element) {
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

	public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
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

	public void addModuleToHierarchy(final Object dbInfo, final Module m) {
		String[] names = m.getName().split("\\.");

		ArrayList<ModulesItem> currentList = hierarchicalCache;
		ModulesItem currentParent = null;
		for (int i = 0; i < names.length; i++) {
			ModulesItem currentItem = null;
			for (ModulesItem item : currentList) {
				if (item.getShownName().equals(names[i])) {
					currentItem = item;
					break;
				}
			}
			if (currentItem == null) {
				if (i == names.length - 1) {
          currentItem = new ModulesItem(dbInfo, names[i], m, currentParent);
        } else {
          currentItem = new ModulesItem(dbInfo, names[i], null, currentParent);
        }
				currentList.add(currentItem);
			} else {
				if (i == names.length - 1) { // Maybe we have newer information
					if (currentItem.getModule() == null) {
            currentItem.setModule(m);
          }
				}
			}
			currentList = currentItem.getModulesArrayList();
			currentParent = currentItem;
		}
	}

	public boolean getHierarchical() {
		return this.isHierarchical;
	}

	public void setHierarchical(final boolean isH) {
		this.isHierarchical = isH;
	}
}
