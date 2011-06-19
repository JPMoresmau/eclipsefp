package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONArray;
import org.json.JSONObject;

public class HoogleResultConstructor extends HoogleResult {
	PackageIdentifier pkg;
	String mod;
	Declaration decl;
	Constructor con;

	public HoogleResultConstructor(PackageIdentifier pkg, String mod, Declaration decl, Constructor con) {
		setType(HoogleResultType.CONSTRUCTOR);
		this.pkg = pkg;
		this.mod = mod;
		this.decl = decl;
		this.con = con;
	}

	public HoogleResultConstructor(JSONObject o) throws Exception {
		setType(HoogleResultType.CONSTRUCTOR);
		JSONArray result = o.getJSONArray("result");
		this.pkg = new PackageIdentifier(result.getJSONObject(0));
		this.mod = result.getString(1);
		this.decl = Declaration.fromJSON(result.getJSONObject(2));
		this.con = new Constructor(result.getJSONObject(3));
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
	
	public Constructor getConstructor() {
		return this.con;
	}

	@Override
	public String getName() {
		return this.con.getName();
	}

}
