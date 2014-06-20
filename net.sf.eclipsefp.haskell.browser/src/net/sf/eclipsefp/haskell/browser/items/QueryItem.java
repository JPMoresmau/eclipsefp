/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.items;

import java.util.ArrayList;

/**
 * Represents a declaration and all the possible packages
 * and modules where it can be found.
 * 
 * @author Alejandro Serrano
 */
public class QueryItem {

	Declaration declaration;
	ArrayList<PackageIdentifier> packages;
	ArrayList<QueryItem> innerItems;

	public QueryItem(Declaration declaration) {
		this.declaration = declaration;
		this.packages = new ArrayList<>();
		this.innerItems = new ArrayList<>();
	}

	public QueryItem(Packaged<Declaration> pDecl) {
		this(pDecl.getElement());
		this.addPackage(pDecl.getPackage());
	}
	
	public Declaration getDeclaration() {
		return this.declaration;
	}

	public DeclarationType getType() {
		return this.declaration.getType();
	}

	public String getName() {
		return this.declaration.getName();
	}

	public boolean isSimilarTo(Declaration otherDecl) {
		if (this.declaration.getType() == otherDecl.getType()) {
			if (this.declaration.type == DeclarationType.INSTANCE) 
				return this.declaration.getCompleteDefinition().equals(otherDecl.getCompleteDefinition());
			else
				return this.declaration.getName().equals(otherDecl.getName());
		} else {
			return false;
		}
	}

	public ArrayList<PackageIdentifier> getPackages() {
		return this.packages;
	}

	public void addPackage(PackageIdentifier pkg) {
		this.packages.add(pkg);
	}

	public ArrayList<QueryItem> getInnerItems() {
		return this.innerItems;
	}

	public void addInnerItem(QueryItem item) {
		this.innerItems.add(item);
	}

	public static ArrayList<QueryItem> convertToQueryItem(Packaged<Declaration>[] decls) {
		// Remove duplication of items
		ArrayList<QueryItem> items = new ArrayList<>();
		removeDuplicateQueryItems(decls, items);
		
		// Separate instances, classes, gadts and other things
		ArrayList<QueryItem> instances = new ArrayList<>();
		ArrayList<QueryItem> classes = new ArrayList<>();
		ArrayList<QueryItem> gadts = new ArrayList<>();
		ArrayList<QueryItem> allOthers = new ArrayList<>();
		
		for (QueryItem item : items) {
			switch(item.getType()) {
			case INSTANCE:
				instances.add(item);
				break;
			case TYPE_CLASS:
				classes.add(item);
				break;
			case DATA_TYPE:
			case NEW_TYPE:
			case TYPE_SYNONYM:
				gadts.add(item);
				break;
			default:
				allOthers.add(item);
				break;
			}
		}
		
		// Add instances to the corresponding item
		ArrayList<QueryItem> noItemInstances = new ArrayList<>();
		for (QueryItem item : instances) {
			boolean added = false;
			// Try to add to typeclasses
			Instance inst = (Instance)item.getDeclaration();
			for (QueryItem tclass : classes) {
				if (tclass.getName().equals(inst.getName())) {
					tclass.addInnerItem(item);
					added = true;
					// We should only find a typeclass this way
					break;
				}
			}
			// Try to add to types
			for (QueryItem gadt : gadts) {
				String gadtName = gadt.getName();
				// The type must be in one of the variables part
				for (String tvar : inst.getTypeVariables()) {
					// Split in tokens and compare
					String[] tvarElts = tvar.replace('(', ' ').replace(')', ' ').split("\\s");
					for (String tvarElt : tvarElts) {
						if (tvarElt.equals(gadtName)) {
							gadt.addInnerItem(item);
							added = true;
						}
					}
				}
			}
			// For instances that went nowhere
			if (!added)
				noItemInstances.add(item);
		}
		
		// Add the rest of elements for returning
		noItemInstances.addAll(classes);
		noItemInstances.addAll(gadts);
		noItemInstances.addAll(allOthers);
		return noItemInstances;
	}
	
	private static void removeDuplicateQueryItems(Packaged<Declaration>[] decls, ArrayList<QueryItem> items) {
		for (Packaged<Declaration> decl : decls) {
			QueryItem similar = null;
			for (QueryItem item : items) {
				if (item.isSimilarTo(decl.getElement())) {
					similar = item;
					break;
				}
			}
			if (similar == null) {
				// New item to add
				QueryItem newItem = new QueryItem(decl);
				items.add(newItem);
			} else {
				similar.addPackage(decl.getPackage());
			}
		}
	}
}
