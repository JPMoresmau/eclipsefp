package net.sf.eclipsefp.haskell.browser.items;

import java.util.ArrayList;

import org.json.JSONArray;
import org.json.JSONObject;

public class HoogleResultDeclaration extends HoogleResult {
	ArrayList<PackageIdentifier> pkg;
	String mod;
	Declaration decl;

	public HoogleResultDeclaration(PackageIdentifier pkg, String mod, Declaration decl) {
		setType(HoogleResultType.DECLARATION);
		this.pkg = new ArrayList<PackageIdentifier>();
		this.pkg.add(pkg);
		this.mod = mod;
		this.decl = decl;
	}

	public HoogleResultDeclaration(JSONObject o) throws Exception {
		setType(HoogleResultType.DECLARATION);
		JSONArray results = o.getJSONArray("results");
		// Get info from first result
		JSONArray first_result = results.getJSONArray(0);
		this.mod = first_result.getString(1);
		this.decl = Declaration.fromJSON(first_result.getJSONObject(2));
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

	@Override
	public String getName() {
		return this.decl.getName();
	}
}
