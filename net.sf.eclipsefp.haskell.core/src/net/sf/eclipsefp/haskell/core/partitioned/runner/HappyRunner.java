package net.sf.eclipsefp.haskell.core.partitioned.runner;


/**
 * Runner for Happy parser tool.
 *
 * @author Alejandro Serrano
 */
public class HappyRunner extends PartitionedRunner {
  private static String fullPath;

  public static void setFullPath( final String path ) {
   fullPath = path;
 }

 @Override
 public String getExecutableName() {
   if (fullPath!=null && fullPath.length()>0){
     return fullPath;
   }
		return "happy"; //$NON-NLS-1$
	}


}
