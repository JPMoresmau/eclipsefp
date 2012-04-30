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
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.buildwrapper.types.Module;
import net.sf.eclipsefp.haskell.buildwrapper.types.UsageResults;
import net.sf.eclipsefp.haskell.buildwrapper.types.UsageResults.UsageLocation;
import net.sf.eclipsefp.haskell.buildwrapper.util.BWText;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.json.JSONArray;
import org.json.JSONException;


/**
 * @author JP Moresmau
 *
 */
public class UsageDB {
	private Connection conn;

	
	public UsageDB(){
		IPath p=BuildWrapperPlugin.getDefault().getStateLocation().append("usage.db");
		File f=p.toFile();
		f.getParentFile().mkdirs();
		try {
			Class.forName("org.sqlite.JDBC");
			conn =
			      DriverManager.getConnection("jdbc:sqlite:"+f.getAbsolutePath());
			conn.setAutoCommit(true);
			Statement s=conn.createStatement();
			try {
				s.executeUpdate("PRAGMA foreign_keys = ON;");
			}  finally {
				s.close();
			}
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
	
	protected void setup() throws SQLException{
		checkConnection();
		Statement s=conn.createStatement();
		try {
			s.execute("create table if not exists files (fileid INTEGER PRIMARY KEY ASC,project TEXT not null, name TEXT not null)");
			s.execute("create unique index if not exists filenames on files (project, name)");
			
			s.execute("create table if not exists modules (moduleid INTEGER PRIMARY KEY ASC,package TEXT not null, module TEXT not null,fileid INTEGER,foreign key (fileid) references files(fileid) on delete set null)");
			s.execute("create unique index if not exists modulenames on modules (package,module)");
			
			//s.execute("create index if not exists modulefiles on modules (fileid)");
			
			s.execute("create table if not exists module_usages (moduleid INTEGER not null,fileid INTEGER not null,location TEXT not null,foreign key (fileid) references files(fileid) on delete cascade,foreign key (moduleid) references modules(moduleid) on delete cascade)");
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
	}
	
	public void setModuleUsages(long fileid,Collection<ModuleUsage> usages) throws SQLException{
		checkConnection();
		PreparedStatement ps=conn.prepareStatement("insert into module_usages values(?,?,?)");
		try {
			for (ModuleUsage usg:usages){
				ps.setLong(1, usg.getModuleID());
				ps.setLong(2, fileid);
				ps.setString(3, usg.getLocation());
				ps.addBatch();
			}
			ps.executeBatch();
		} finally {
			ps.close();
		}
	}
	
	public long getModuleID(String pkg,String module,Long fileID) throws SQLException {
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
			 ps=conn.prepareStatement("insert into modules (package,module,fileid) values(?,?,?)");
			 try {
				ps.setString(1, pkg);
				ps.setString(2, module);
				if (fileID!=null){
					ps.setLong(3, fileID);
				} else {
					ps.setNull(3, Types.NUMERIC);
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
		}
		return moduleID;
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
	
	public UsageResults getModuleReferences(String pkg,String module) throws SQLException {
		checkConnection();
		StringBuilder sb=new StringBuilder("select mu.fileid,mu.location from module_usages mu, modules m where mu.moduleid=m.moduleid and m.module=?");
		if (pkg!=null){
			sb.append(" and m.package=?");
		}
		PreparedStatement ps=conn.prepareStatement(sb.toString());
		Map<Long,Collection<UsageLocation>> m=new HashMap<Long, Collection<UsageLocation>>();
		try {
			ps.setString(1, module);
			if (pkg!=null){
				ps.setString(2, pkg);
			}
			ResultSet rs=ps.executeQuery();
			try {
				while (rs.next()){
					long fileid=rs.getLong(1);
					Collection<UsageLocation> locs=m.get(fileid);
					if (locs==null){
						locs=new ArrayList<UsageResults.UsageLocation>();
						m.put(fileid,locs);
					}
					//IProject p=ResourcesPlugin.getWorkspace().getRoot().getProject(project);
					//if (p!=null){
						//IFile f=p.getFile(name);
						String loc=rs.getString(2);
						try {
							Location l=new net.sf.eclipsefp.haskell.buildwrapper.types.Location("", new JSONArray(loc));
							locs.add(new UsageLocation(l));
							
						} catch (JSONException je){
							BuildWrapperPlugin.logError(je.getLocalizedMessage(), je);
						}
					//}
				}
			}finally {
				rs.close();
			}	
		} finally {
			ps.close();
		}
		UsageResults ret=new UsageResults();
		for (Long fileid:m.keySet()){
			IFile f=getFile(fileid);
			ret.put(f, m.get(fileid));
		}
		return ret;
	}
	
}
