/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.items;

import java.util.ArrayList;

import org.json.JSONArray;
import org.json.JSONObject;

/**
 * Represents the information from a constructor returned
 * by a Hoogle search.
 * 
 * @author Alejandro Serrano
 */
public class HoogleResultConstructor extends HoogleResult {
	ArrayList<PackageIdentifier> pkg;
	String mod;
	Declaration decl;
	Constructor con;

	public HoogleResultConstructor(PackageIdentifier pkg, String mod, Declaration decl, Constructor con) {
		setType(HoogleResultType.CONSTRUCTOR);
		this.pkg = new ArrayList<PackageIdentifier>();
		this.pkg.add(pkg);
		this.mod = mod;
		this.decl = decl;
		this.con = con;
	}

	public HoogleResultConstructor(JSONObject o) throws Exception {
		setType(HoogleResultType.CONSTRUCTOR);
		JSONArray results = o.getJSONArray("results");
		// Get info from first result
		JSONArray first_result = results.getJSONArray(0);
		this.mod = first_result.getString(1);
		this.decl = Declaration.fromJSON(first_result.getJSONObject(2));
		this.con = new Constructor(first_result.getJSONObject(3));
		// Add packages
		this.pkg = new ArrayList<PackageIdentifier>();
		for (int i = 0; i < results.length(); i++) {
			JSONArray result = results.getJSONArray(i);
			this.pkg.add(new PackageIdentifier(result.getJSONObject(0)));;
		}
	}

	public ArrayList<PackageIdentifier> getPackageIdentifiers() {
		return this.pkg;
	}
	
	public String getModule() {
		return this.mod;
	}
	
	public Declaration getDeclaration() {
		return this.decl;
	}
	
	public Constructor getConstructor() {
		return this.con;
	}

	@Override
	public String getName() {
		return this.con.getName();
	}

	@Override
	public String getCompleteDefinition() {
		return this.con.getCompleteDefinition();
	}
}
