package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONArray;
import org.json.JSONObject;

public class HoogleResultDeclaration extends HoogleResult {
	PackageIdentifier pkg;
	String mod;
	Declaration decl;

	public HoogleResultDeclaration(PackageIdentifier pkg, String mod, Declaration decl) {
		setType(HoogleResultType.DECLARATION);
		this.pkg = pkg;
		this.mod = mod;
		this.decl = decl;
	}

	public HoogleResultDeclaration(JSONObject o) throws Exception {
		setType(HoogleResultType.DECLARATION);
		JSONArray result = o.getJSONArray("result");
		this.pkg = new PackageIdentifier(result.getJSONObject(0));
		this.mod = result.getString(1);
		this.decl = Declaration.fromJSON(result.getJSONObject(2));
	}

	public PackageIdentifier getPackageIdentifier() {
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
