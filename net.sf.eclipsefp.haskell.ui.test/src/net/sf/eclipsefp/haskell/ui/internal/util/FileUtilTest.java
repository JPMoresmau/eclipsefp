package net.sf.eclipsefp.haskell.ui.internal.util;

import java.io.File;
import java.io.IOException;
import net.sf.eclipsefp.haskell.core.util.FileUtil;
import junit.framework.TestCase;


public class FileUtilTest extends TestCase{


  public FileUtilTest( final String name ) {
    super( name );
  }

  public void testDeleteRecursively(){
    try {
      File tmp=File.createTempFile( "filetogetdir", ".tmp" );
      File tmpDir=tmp.getParentFile();
      File tmpDir1=new File(tmpDir,"tmpDir");
      File tmpFile1=File.createTempFile("tmpFile1",".tmp",tmpDir1);
      File tmpDir11=new File(tmpDir1,"tmpDir11");
      tmpDir11.mkdirs();
      File tmpFile111=File.createTempFile("tmpFile111",".tmp",tmpDir11);
      File tmpDir12=new File(tmpDir1,"tmpDir12");
      tmpDir12.mkdirs();
      File tmpFile121=File.createTempFile("tmpFile121",".tmp",tmpDir12);


      assertTrue(tmpDir1.exists());
      assertTrue(tmpFile1.exists());
      assertTrue(tmpDir11.exists());
      assertTrue(tmpFile111.exists());
      assertTrue(tmpDir12.exists());
      assertTrue(tmpFile121.exists());
      boolean deleted=FileUtil.deleteRecursively(tmpDir1);
      assertTrue(deleted);
      assertFalse(tmpDir1.exists());
      assertFalse(tmpFile1.exists());
      assertFalse(tmpDir11.exists());
      assertFalse(tmpFile111.exists());
      assertFalse(tmpDir12.exists());
      assertFalse(tmpFile121.exists());
    } catch (IOException ioe){
      fail(ioe.getLocalizedMessage());
    }

  }


}
