/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.usage;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.Module;
import net.sf.eclipsefp.haskell.buildwrapper.types.SearchResultLocation;
import net.sf.eclipsefp.haskell.buildwrapper.types.UsageResults;
import net.sf.eclipsefp.haskell.buildwrapper.util.BWText;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.json.JSONArray;
import org.json.JSONException;


/**
 * The DB side (sqlite) of usage queries
 * @author JP Moresmau
 *
 */
public class UsageDB {
	private Connection conn;

	
	public UsageDB(){
		// the db file is inside the project metadata folder
		IPath p=BuildWrapperPlugin.getDefault().getStateLocation().append("usage.db");
		File f=p.toFile();
		f.getParentFile().mkdirs();
		try {
			Class.forName("org.sqlite.JDBC");
			conn =
			      DriverManager.getConnection("jdbc:sqlite:"+f.getAbsolutePath());
			conn.setAutoCommit(true);
			Statement s=conn.createStatement();
			// foreign key support
			try {
				s.executeUpdate("PRAGMA foreign_keys = ON;");
			}  finally {
				s.close();
			}
			// we do explicit commits for performance and coherence
			conn.setAutoCommit(false);
			setup();
		} catch (Exception e){
			BuildWrapperPlugin.logError(BWText.error_setup_db, e);
		}
	}
	
	public void close(){
		if (conn!=null){
			try {
				conn.close();
			} catch (SQLException sqle){
				BuildWrapperPlugin.logError(BWText.error_db, sqle);
			}
			conn=null;
		}
	}
	
	public void commit() throws SQLException{
		checkConnection();
		conn.commit();
	}
	
	public boolean isValid(){
		return conn!=null;
	}
	
	protected void checkConnection() throws SQLException{
		if (conn==null){
			throw new SQLException(BWText.error_no_db);
		}
	}
	
	/**
	 * create tables and indices if needed
	 * @throws SQLException
	 */
	protected void setup() throws SQLException{
		checkConnection();
		Statement s=conn.createStatement();
		try {
			s.execute("create table if not exists files (fileid INTEGER PRIMARY KEY ASC,project TEXT not null, name TEXT not null)");
			s.execute("create unique index if not exists filenames on files (project, name)");
			
			s.execute("create table if not exists modules (moduleid INTEGER PRIMARY KEY ASC,package TEXT not null, module TEXT not null,fileid INTEGER,location TEXT,foreign key (fileid) references files(fileid) on delete set null)");
			s.execute("create unique index if not exists modulenames on modules (package,module)");
			
			//s.execute("create index if not exists modulefiles on modules (fileid)");
			
			s.execute("create table if not exists module_usages (moduleid INTEGER not null,fileid INTEGER not null,section TEXT not null,location TEXT not null,foreign key (fileid) references files(fileid) on delete cascade,foreign key (moduleid) references modules(moduleid) on delete cascade)");
		
			s.execute("create table if not exists symbols(symbolid INTEGER PRIMARY KEY ASC,symbol TEXT,type INTEGER,moduleid INTEGER not null,fileid INTEGER,section TEXT,location TEXT,foreign key (fileid) references files(fileid) on delete set null,foreign key (moduleid) references modules(moduleid) on delete set null)");
			s.execute("create index if not exists symbolnames on symbols (symbol,type,moduleid)");
			
			s.execute("create table if not exists symbol_usages(symbolid INTEGER not null,fileid INTEGER not null,section TEXT not null,location TEXT not null,foreign key (symbolid) references symbols(symbolid) on delete cascade,foreign key (fileid) references files(fileid) on delete cascade)");
			
			
		} finally {
			s.close();
		}
		conn.commit();
	}
	
	public long getFileID(IFile f) throws SQLException{
		checkConnection();
		PreparedStatement ps=conn.prepareStatement("select fileid from files where project=? and name=?");
		Long fileID=null;
		try {
			ps.setString(1, f.getProject().getName());
			ps.setString(2, f.getProjectRelativePath().toPortableString());
			ResultSet rs=ps.executeQuery();
			try {
				if (rs.next()){
					fileID=rs.getLong(1);
				}
			} finally {
				rs.close();
			}
			
		} finally {
			ps.close();
		}
		if (fileID==null){
			 ps=conn.prepareStatement("insert into files (project,name) values(?,?)");
			 try {
				ps.setString(1, f.getProject().getName());
				ps.setString(2, f.getProjectRelativePath().toPortableString());
				ps.execute();
				ResultSet rs=ps.getGeneratedKeys();
				try {
					rs.next();
					fileID=rs.getLong(1);
				} finally {
					rs.close();
				}
			 } finally {
				ps.close();
			}
		}
		return fileID;
	}
	
	public void removeFile(IFile f) throws SQLException{
		checkConnection();
		PreparedStatement ps=conn.prepareStatement("select fileid from files where project=? and name=?");
		Long fileID=null;
		try {
			ps.setString(1, f.getProject().getName());
			ps.setString(2, f.getProjectRelativePath().toPortableString());
			ResultSet rs=ps.executeQuery();
			try {
				if (rs.next()){
					fileID=rs.getLong(1);
				}
			} finally {
				rs.close();
			}
			
		} finally {
			ps.close();
		}
		if (fileID!=null){
			ps=conn.prepareStatement("delete from files where fileid=?");
			try {
				ps.setLong(1, fileID);
				ps.executeUpdate();
			} finally {
				ps.close();
			}
			
		}
	}
	
	public void clearUsageInFile(long fileid) throws SQLException{
		checkConnection();
		PreparedStatement ps=conn.prepareStatement("delete from module_usages where fileid=?");
		try {
			ps.setLong(1, fileid);
			ps.executeUpdate();
		} finally {
			ps.close();
		}
		ps=conn.prepareStatement("delete from symbol_usages where fileid=?");
		try {
			ps.setLong(1, fileid);
			ps.executeUpdate();
		} finally {
			ps.close();
		}
		ps=conn.prepareStatement("update symbols set fileid=null,section=null,location=null where fileid=?");
		try {
			ps.setLong(1, fileid);
			ps.executeUpdate();
		} finally {
			ps.close();
		}
	}
	
	public void setModuleUsages(long fileid,Collection<ObjectUsage> usages) throws SQLException{
		checkConnection();
		PreparedStatement ps=conn.prepareStatement("insert into module_usages values(?,?,?,?)");
		try {
			for (ObjectUsage usg:usages){
				ps.setLong(1, usg.getObjectID());
				ps.setLong(2, fileid);
				ps.setString(3, usg.getSection());
				ps.setString(4, usg.getLocation());
				ps.addBatch();
			}
			ps.executeBatch();
		} finally {
			ps.close();
		}
	}
	
	public void setSymbolUsages(long fileid,Collection<ObjectUsage> usages) throws SQLException{
		checkConnection();
		PreparedStatement ps=conn.prepareStatement("insert into symbol_usages values(?,?,?,?)");
		try {
			for (ObjectUsage usg:usages){
				ps.setLong(1, usg.getObjectID());
				ps.setLong(2, fileid);
				ps.setString(3, usg.getSection());
				ps.setString(4, usg.getLocation());
				ps.addBatch();
			}
			ps.executeBatch();
		} finally {
			ps.close();
		}
	}
	
	public long getModuleID(String pkg,String module,Long fileID,String loc) throws SQLException {
		checkConnection();
		PreparedStatement ps=conn.prepareStatement("select moduleid from modules where package=? and module=?");
		Long moduleID=null;
		try {
			ps.setString(1, pkg);
			ps.setString(2, module);
			ResultSet rs=ps.executeQuery();
			try {
				if (rs.next()){
					moduleID=rs.getLong(1);
				}
			} finally {
				rs.close();
			}
			
		} finally {
			ps.close();
		}
		if (moduleID==null){
			 ps=conn.prepareStatement("insert into modules (package,module,fileid,location) values(?,?,?,?)");
			 try {
				ps.setString(1, pkg);
				ps.setString(2, module);
				if (fileID!=null){
					ps.setLong(3, fileID);
				} else {
					ps.setNull(3, Types.NUMERIC);
				}
				if (loc!=null){
					ps.setString(4, loc);
				} else {
					ps.setNull(4, Types.VARCHAR);
				}
				ps.execute();
				ResultSet rs=ps.getGeneratedKeys();
				try {
					rs.next();
					moduleID=rs.getLong(1);
				} finally {
					rs.close();
				}
			 } finally {
				ps.close();
			}
		} else if (fileID!=null){
			 ps=conn.prepareStatement("update modules set fileid=?,location=? where moduleid=?");
			 try {
				
				ps.setLong(1, fileID);
				if (loc!=null){
					ps.setString(2, loc);
				} else {
					ps.setNull(2, Types.VARCHAR);
				}
				ps.setLong(3, moduleID);
				ps.execute();
			} finally {
				ps.close();
			}
		}
		return moduleID;
	}
	
	public long getSymbolID(long moduleid,String symbol,int type,Long fileID,String section,String loc) throws SQLException {
		checkConnection();
		PreparedStatement ps=conn.prepareStatement("select symbolid from symbols where moduleid=? and symbol=? and type=?");
		Long symbolID=null;
		try {
			ps.setLong(1, moduleid);
			ps.setString(2, symbol);
			ps.setInt(3, type);
			ResultSet rs=ps.executeQuery();
			try {
				if (rs.next()){
					symbolID=rs.getLong(1);
				}
			} finally {
				rs.close();
			}
			
		} finally {
			ps.close();
		}
		if (symbolID==null){
			 ps=conn.prepareStatement("insert into symbols (symbol,type,moduleid,fileid,section,location) values(?,?,?,?,?,?)");
			 try {
				ps.setString(1, symbol);
				ps.setInt(2, type);
				ps.setLong(3, moduleid);
				if (fileID!=null){
					ps.setLong(4, fileID);
				} else {
					ps.setNull(4, Types.NUMERIC);
				}
				if (section!=null){
					ps.setString(5, section);
				} else {
					ps.setNull(5, Types.VARCHAR);
				}
				if (loc!=null){
					ps.setString(6, loc);
				} else {
					ps.setNull(6, Types.VARCHAR);
				}
				ps.execute();
				ResultSet rs=ps.getGeneratedKeys();
				try {
					rs.next();
					symbolID=rs.getLong(1);
				} finally {
					rs.close();
				}
			 } finally {
				ps.close();
			}
		} else if (fileID!=null){
			 ps=conn.prepareStatement("update symbols set fileid=?,section=?,location=? where symbolid=?");
			 try {
				
				ps.setLong(1, fileID);
				if (section!=null){
					ps.setString(2, section);
				} else {
					ps.setNull(2, Types.VARCHAR);
				}
				if (loc!=null){
					ps.setString(3, loc);
				} else {
					ps.setNull(3, Types.VARCHAR);
				}
				ps.setLong(4, symbolID);
				ps.execute();
			} finally {
				ps.close();
			}
		}
		return symbolID;
	}
	
	public List<Module> listLocalModules() throws SQLException {
		checkConnection();
		List<Module> ret=new ArrayList<Module>();
		PreparedStatement ps=conn.prepareStatement("select moduleid,package,module,fileid from modules where fileid is not null");
		try {
			ResultSet rs=ps.executeQuery();
			try {
				while (rs.next()){
					long moduleID=rs.getLong(1);
					String packageName=rs.getString(2);
					String moduleName=rs.getString(3);
					long fileid=rs.getLong(4);
					Module mod=new Module(moduleID,packageName,moduleName,fileid);
					ret.add(mod);
				}
			} finally {
				rs.close();
			}
		} finally {
			ps.close();
		}
		
		return ret;
	}
	
	public IFile getFile(Long fileid) throws SQLException {
		if (fileid==null){
			return null;
		}
		checkConnection();
		PreparedStatement ps=conn.prepareStatement("select project,name from files where fileid =?");
		try {
			ps.setLong(1, fileid);
			ResultSet rs=ps.executeQuery();
			try {
				if (rs.next()){
					
					String project=rs.getString(1);
					String name=rs.getString(2);
					IProject p=ResourcesPlugin.getWorkspace().getRoot().getProject(project);
					if (p!=null){
						return p.getFile(name);
					}
				}
			} finally {
				rs.close();
			}
		} finally {
			ps.close();
		}
		return null;
	}
	
	public boolean knowsProject(String project) throws SQLException{
		checkConnection();
		PreparedStatement ps=conn.prepareStatement("select project from files where project=?");
		try {
			ps.setString(1, project);
			ResultSet rs=ps.executeQuery();
			try {
				return rs.next();
			}finally {
				rs.close();
			}	
		} finally {
			ps.close();
		}
		
	}

	public UsageResults getModuleDefinitions(String pkg,String module,IProject p,boolean exact) throws SQLException {
		checkConnection();
		StringBuilder sb=new StringBuilder("select m.fileid,'module ' || module,m.location,1 from modules m");
		if (p!=null){
			sb.append(",files f");
		}
		if (exact){
			sb.append(" where m.module=?");
		} else {
			sb.append(" where m.module LIKE ? ESCAPE '\\'");
		}
		if (pkg!=null){
			sb.append(" and m.package=?");
		}
		if (p!=null){
			sb.append(" and f.fileid=m.fileid and f.project=?");
		}
		sb.append(" and m.location is not null");
		return getUsageResults(pkg, module, p, sb.toString());
	}
	
	public List<String> findModules(String pkg,String module,IProject p) throws SQLException {
		checkConnection();
		StringBuilder sb=new StringBuilder("select module from modules m");
		if (p!=null){
			sb.append(",files f");
		}
		sb.append(" where m.module LIKE ? ESCAPE '\\'");
		if (pkg!=null){
			sb.append(" and m.package=?");
		}
		if (p!=null){
			sb.append(" and f.fileid=m.fileid and f.project=?");
		}
		PreparedStatement ps=conn.prepareStatement(sb.toString());
		List<String> ret=new ArrayList<String>();
		try {
			int ix=1;
			if (module!=null){
				ps.setString(ix++, module);
			}
			if (pkg!=null){
				ps.setString(ix++, pkg);
			}
			if (p!=null){
				ps.setString(ix++, p.getName());
			}

			ResultSet rs=ps.executeQuery();
			try {
				while (rs.next()){
					ret.add(rs.getString(1));
				}
			} finally {
				rs.close();
			}
		} finally {
			ps.close();
		}
		return ret;
	}
	
	public UsageResults getSymbolDefinitions(String pkg,String module,String symbol,int type,IProject p,boolean exact) throws SQLException {
		checkConnection();
		StringBuilder sb=new StringBuilder("select s.fileid,s.section,s.location,1 from symbols s,modules m");
		if (p!=null){
			sb.append(",files f");
		}
		sb.append(" where ");
		if (module!=null){
			sb.append("m.module=? and");
		}
		sb.append(" m.moduleid=s.moduleid");
		if (pkg!=null){
			sb.append(" and m.package=?");
		}
		if (p!=null){
			sb.append(" and f.fileid=s.fileid and f.project=?");
		}
		if (exact){
			sb.append(" and s.symbol=?");
		} else {
			sb.append(" and s.symbol LIKE ? ESCAPE '\\'");
		}
		if (type>0){
			sb.append(" and s.type=?");
		}
		sb.append(" and s.location is not null");
		return getUsageResults(pkg, module, p,symbol,type, sb.toString());
	}
			
	public UsageResults getModuleReferences(String pkg,String module,IProject p,boolean exact) throws SQLException {
		checkConnection();
		StringBuilder sb=new StringBuilder("select mu.fileid,mu.section,mu.location,0 from module_usages mu, modules m");
		if (p!=null){
			sb.append(",files f");
		}
		sb.append(" where mu.moduleid=m.moduleid");
		if (exact){
			sb.append(" and m.module=?");
		} else {
			sb.append(" and m.module LIKE ? ESCAPE '\\'");
		}
		if (pkg!=null){
			sb.append(" and m.package=?");
		}
		if (p!=null){
			sb.append(" and f.fileid=mu.fileid and f.project=?");
		}
		return getUsageResults(pkg, module, p, sb.toString());	
	}
	
	public UsageResults getSymbolReferences(String pkg,String module,String symbol,int type,IProject p,boolean exact) throws SQLException {
		checkConnection();
		StringBuilder sb=new StringBuilder("select su.fileid,su.section,su.location,0 from symbol_usages su,symbols s, modules m");
		if (p!=null){
			sb.append(",files f");
		}
		sb.append(" where s.symbolid=su.symbolid and s.moduleid=m.moduleid");
		if (module!=null){
			sb.append(" and m.module=?");
		}
		if (pkg!=null){
			sb.append(" and m.package=?");
		}
		if (p!=null){
			sb.append(" and f.fileid=su.fileid and f.project=?");
		}
		if (exact){
			sb.append(" and s.symbol=?");
		} else {
			sb.append(" and s.symbol LIKE ? ESCAPE '\\'");
		}
		if (type>0){
			sb.append(" and s.type=?");
		}
		return getUsageResults(pkg, module, p,symbol,type, sb.toString());	
	}
	
	private UsageResults getUsageResults(String pkg,String module,IProject p,String query) throws SQLException{
		PreparedStatement ps=conn.prepareStatement(query);
		
		try {
			int ix=1;
			if (module!=null){
				ps.setString(ix++, module);
			}
			if (pkg!=null){
				ps.setString(ix++, pkg);
			}
			if (p!=null){
				ps.setString(ix++, p.getName());
			}
			return getUsageResults(ps,query);
		} finally {
			ps.close();
		}
	}
	
	private UsageResults getUsageResults(String pkg,String module,IProject p,String symbol,int type,String query) throws SQLException{
		PreparedStatement ps=conn.prepareStatement(query);
		
		try {
			int ix=1;
			if (module!=null){
				ps.setString(ix++, module);
			}
			if (pkg!=null){
				ps.setString(ix++, pkg);
			}
			if (p!=null){
				ps.setString(ix++, p.getName());
			}
			ps.setString(ix++,symbol);
			if (type>0){
				ps.setInt(ix++, type);
			}
			return getUsageResults(ps,query);
		} finally {
			ps.close();
		}
	}
		
	private UsageResults getUsageResults(PreparedStatement ps,String query) throws SQLException{
		Map<Long,Map<String,Collection<SearchResultLocation>>> m=new HashMap<Long, Map<String,Collection<SearchResultLocation>>>();
	
		ResultSet rs=ps.executeQuery();
		try {
			while (rs.next()){
				long fileid=rs.getLong(1);
				Map<String,Collection<SearchResultLocation>> sections=m.get(fileid);
				if (sections==null){
					sections=new HashMap<String, Collection<SearchResultLocation>>();
					m.put(fileid, sections);
				}
				String section=rs.getString(2);
				Collection<SearchResultLocation> locs=sections.get(section);
				if (locs==null){
					locs=new ArrayList<SearchResultLocation>();
					sections.put(section,locs);
				}
				//IProject p=ResourcesPlugin.getWorkspace().getRoot().getProject(project);
				//if (p!=null){
					//IFile f=p.getFile(name);
					String loc=rs.getString(3);
					boolean def=rs.getBoolean(4);
					try {
						SearchResultLocation l=new SearchResultLocation(null, new JSONArray(loc));
						l.setDefinition(def);
						locs.add(l);
						
					} catch (JSONException je){
						BuildWrapperPlugin.logError(je.getLocalizedMessage(), je);
					}
				//}
			}
		}finally {
			rs.close();
		}	

		UsageResults ret=new UsageResults();
		for (Long fileid:m.keySet()){
			IFile f=getFile(fileid);
			ret.put(f, m.get(fileid));
		}
		return ret;
	}
}
