package net.sf.eclipsefp.haskell.core.cabalmodel;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.List;
import java.util.Map;
import junit.framework.TestCase;


public class CabalModelTest extends TestCase {

  public CabalModelTest( final String name ) {
    super( name );
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();

  }

  private String getContent(final String fileName){
    try {
      InputStream is=getClass().getResourceAsStream( fileName);
      ByteArrayOutputStream baos=new ByteArrayOutputStream();
      int c=-1;
      while ((c=is.read())!=-1){
        baos.write(c);
      }
      is.close();
      return new String(baos.toByteArray(),"UTF8");
    } catch (Exception e){
      e.printStackTrace();
      fail(e.getLocalizedMessage());
    }
    return null;
  }

    public void testParseExample1(){
      String content3=getContent( "Example1.cabal" );
      PackageDescription pd=PackageDescriptionLoader.load( content3 );
      PackageDescriptionStanza[] pdss=pd.getStanzas();
      assertNotNull(pdss);
      assertEquals(2,pdss.length);
      assertTrue(pdss[0] instanceof GeneralStanza);
      assertEquals(0,pdss[0].getIndent());
      assertEquals("HUnit",pdss[0].getName());
      assertEquals(0,pdss[0].getStartLine());
      assertEquals(9,pdss[0].getEndLine());
      assertNotNull(pdss[0].getProperties());
      assertEquals(9,pdss[0].getProperties().size());
      assertEquals("HUnit",pdss[0].getProperties().get( "name"));
      assertEquals("HUnit",pdss[0].getProperties().get( "Name"));
      assertEquals("HUnit",pdss[0].getProperties().get( CabalSyntax.FIELD_NAME));
      ValuePosition vp=pdss[0].getPositions().get(CabalSyntax.FIELD_NAME);
      assertEquals(0,vp.getStartLine());
      assertEquals(1,vp.getEndLine());
      assertEquals(7,vp.getInitialIndent());
      assertEquals(13,vp.getSubsequentIndent());
      assertEquals("1.1.1",pdss[0].getProperties().get( "Version"));
      assertEquals(">= 1.2",pdss[0].getProperties().get( "Cabal-Version"));
      assertEquals("BSD3",pdss[0].getProperties().get( "License"));
      assertEquals("LICENSE",pdss[0].getProperties().get( CabalSyntax.FIELD_LICENSE_FILE));
      assertEquals("Dean Herington",pdss[0].getProperties().get( "Author"));
      assertEquals("A unit testing framework for Haskell",pdss[0].getProperties().get( "Synopsis"));
      assertEquals("http://hunit.sourceforge.net/",pdss[0].getProperties().get( "Homepage"));
      assertEquals("Testing",pdss[0].getProperties().get( CabalSyntax.FIELD_CATEGORY));

      assertEquals(CabalSyntax.SECTION_LIBRARY,pdss[1].getType());
      assertNull(pdss[1].getName());
      assertEquals(2,pdss[1].getIndent());
      assertEquals(10,pdss[1].getStartLine());
      assertEquals(16,pdss[1].getEndLine());
      assertNotNull(pdss[1].getProperties());
      assertEquals(3,pdss[1].getProperties().size());
      assertEquals("base",pdss[1].getProperties().get( "Build-Depends"));
      vp=pdss[1].getPositions().get("Build-Depends");
      assertEquals(11,vp.getStartLine());
      assertEquals(12,vp.getEndLine());
      assertEquals(17,vp.getInitialIndent());
      assertEquals(20,vp.getSubsequentIndent());
      assertEquals("CPP",pdss[1].getProperties().get( "Extensions"));
      assertEquals("Test.HUnit.Base, Test.HUnit.Lang, Test.HUnit.Terminal,"+System.getProperty( "line.separator" )+"Test.HUnit.Text, Test.HUnit",pdss[1].getProperties().get( "Exposed-modules"));
  }

  public void testModifyExample1(){
    String content3=getContent( "Small.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    PackageDescriptionStanza[] pdss=pd.getStanzas();
    PackageDescriptionStanza pds=pdss[0];
    assertEquals("HUnit",pds.getProperties().get( CabalSyntax.FIELD_NAME ));
    RealValuePosition rvp=pds.update( CabalSyntax.FIELD_NAME, "JP" );
    assertEquals("JP"+System.getProperty( "line.separator" ),rvp.getRealValue());
    assertEquals(0,rvp.getStartLine());
    assertEquals(1,rvp.getEndLine());
    assertEquals(13,rvp.getInitialIndent());
    rvp=pds.update( CabalSyntax.FIELD_NAME, "JP2" );
    assertEquals("JP2"+System.getProperty( "line.separator" ),rvp.getRealValue());
    assertEquals(0,rvp.getStartLine());
    assertEquals(1,rvp.getEndLine());
    assertEquals(13,rvp.getInitialIndent());

    rvp=pds.update( CabalSyntax.FIELD_VERSION, "1.0" );
    assertEquals(CabalSyntax.FIELD_VERSION+":     1.0"+System.getProperty( "line.separator" ),rvp.getRealValue());
    assertEquals(1,rvp.getStartLine());
    assertEquals(1,rvp.getEndLine());
    assertEquals(0,rvp.getInitialIndent());

    rvp=pds.update( CabalSyntax.FIELD_VERSION, "1.1.1" );
    assertEquals("1.1.1"+System.getProperty( "line.separator" ),rvp.getRealValue());
    assertEquals(1,rvp.getStartLine());
    assertEquals(2,rvp.getEndLine());
    assertEquals(13,rvp.getInitialIndent());

  }

  public void testParseExample3(){
    String content3=getContent( "Example3.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    PackageDescriptionStanza[] pdss=pd.getStanzas();
    assertNotNull(pdss);
    assertEquals(4,pdss.length);
    assertTrue(pdss[0] instanceof GeneralStanza);
    assertEquals("TestPackage",pdss[0].getName());
    assertEquals(0,pdss[0].getStartLine());
    assertEquals(7,pdss[0].getEndLine());
    assertNotNull(pdss[0].getProperties());
    assertEquals(7,pdss[0].getProperties().size());
    assertEquals("TestPackage",pdss[0].getProperties().get( "name"));
    assertEquals("TestPackage",pdss[0].getProperties().get( "Name"));
    assertEquals("TestPackage",pdss[0].getProperties().get( "NAME"));
    assertEquals("TestPackage",pdss[0].getProperties().get( CabalSyntax.FIELD_NAME));
    assertEquals("0.0",pdss[0].getProperties().get( "Version"));
    assertEquals(">= 1.2",pdss[0].getProperties().get( "Cabal-Version"));
    assertEquals("BSD3",pdss[0].getProperties().get( "License"));
    assertEquals("Angela Author",pdss[0].getProperties().get( "Author"));
    assertEquals("Package with library and two programs",pdss[0].getProperties().get( "Synopsis"));
    assertEquals("Simple",pdss[0].getProperties().get( "Build-Type"));

    assertEquals(CabalSyntax.SECTION_LIBRARY,pdss[1].getType());
    assertNull(pdss[1].getName());
    assertEquals(8,pdss[1].getStartLine());
    assertEquals(11,pdss[1].getEndLine());
    assertNotNull(pdss[1].getProperties());
    assertEquals(2,pdss[1].getProperties().size());
    assertEquals("HUnit",pdss[1].getProperties().get( "Build-Depends"));
    assertEquals("A, B, C",pdss[1].getProperties().get( CabalSyntax.FIELD_EXPOSED_MODULES));

    assertEquals(CabalSyntax.SECTION_EXECUTABLE,pdss[2].getType());
    assertEquals("program1",pdss[2].getName());
    assertEquals(12,pdss[2].getStartLine());
    assertEquals(16,pdss[2].getEndLine());
    assertNotNull(pdss[2].getProperties());
    assertEquals(3,pdss[2].getProperties().size());
    assertEquals("Main.hs",pdss[2].getProperties().get( "Main-Is"));
    assertEquals("prog1",pdss[2].getProperties().get( "Hs-Source-Dirs"));
    assertEquals("A, B",pdss[2].getProperties().get( "Other-Modules"));


    assertEquals(CabalSyntax.SECTION_EXECUTABLE,pdss[3].getType());
    assertEquals("program2",pdss[3].getName());
    assertEquals(17,pdss[3].getStartLine());
    assertEquals(21,pdss[3].getEndLine());
    assertNotNull(pdss[3].getProperties());
    assertEquals(3,pdss[3].getProperties().size());
    assertEquals("Main.hs",pdss[3].getProperties().get( "Main-Is"));
    assertEquals("prog2",pdss[3].getProperties().get( "Hs-Source-Dirs"));
    assertEquals("A, C, Utils",pdss[3].getProperties().get( "Other-Modules"));

  }

  public void testParseSourceRep(){
    String content3=getContent( "SourceRep.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    PackageDescriptionStanza[] pdss=pd.getStanzas();
    assertNotNull(pdss);
    assertEquals(3,pdss.length);
    assertTrue(pdss[0] instanceof GeneralStanza);
    assertEquals("HUnit",pdss[0].getName());

    assertEquals(CabalSyntax.SECTION_SOURCE_REPOSITORY,pdss[1].getType());
    assertEquals("head",pdss[1].getName());
    assertEquals("darcs",pdss[1].getProperties().get("type"));
    assertEquals("http://darcs.haskell.org/cabal/",pdss[1].getProperties().get("location"));


    assertEquals(CabalSyntax.SECTION_SOURCE_REPOSITORY,pdss[2].getType());
    assertEquals("this",pdss[2].getName());
    assertEquals("darcs",pdss[2].getProperties().get("type"));
    assertEquals("http://darcs.haskell.org/cabal-branches/cabal-1.6/",pdss[2].getProperties().get("location"));
    assertEquals("1.6.1",pdss[2].getProperties().get( "tag" ));

  }

  public void testParseExample4(){
    String content3=getContent( "Example4.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    PackageDescriptionStanza[] pdss=pd.getStanzas();
    assertNotNull(pdss);
    assertEquals(8,pdss.length);
    assertTrue(pdss[0] instanceof GeneralStanza);
    assertEquals("Test1",pdss[0].getName());

    assertEquals(CabalSyntax.SECTION_FLAG,pdss[1].getType());
    assertEquals("Debug",pdss[1].getName());
    assertEquals("Enable debug support",pdss[1].getProperties().get( CabalSyntax.FIELD_DESCRIPTION ));
    assertEquals("False",pdss[1].getProperties().get( CabalSyntax.FIELD_DEFAULT ));


    assertEquals(CabalSyntax.SECTION_FLAG,pdss[2].getType());
    assertEquals("WebFrontend",pdss[2].getName());

    assertEquals(CabalSyntax.SECTION_LIBRARY,pdss[3].getType());

    assertEquals(CabalSyntax.SECTION_IF,pdss[4].getType());

    assertEquals(CabalSyntax.SECTION_IF,pdss[5].getType());

    assertEquals(CabalSyntax.SECTION_EXECUTABLE,pdss[6].getType());

    assertEquals(CabalSyntax.SECTION_IF,pdss[7].getType());
  }

  public void testParseExample5(){
    String content3=getContent( "Example5.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    PackageDescriptionStanza[] pdss=pd.getStanzas();
    assertNotNull(pdss);
    assertEquals(3,pdss.length);
    assertTrue(pdss[0] instanceof GeneralStanza);
    assertEquals("Test1",pdss[0].getName());

    assertEquals(CabalSyntax.SECTION_FLAG,pdss[1].getType());
    assertEquals("Debug",pdss[1].getName());
    assertEquals("Enable debug support",pdss[1].getProperties().get( CabalSyntax.FIELD_DESCRIPTION ));
    assertEquals("False",pdss[1].getProperties().get( CabalSyntax.FIELD_DEFAULT ));


    assertEquals(CabalSyntax.SECTION_LIBRARY,pdss[2].getType());

  }

  public void testSpaces(){
    String content3=getContent( "Spaces.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    PackageDescriptionStanza[] pdss=pd.getStanzas();
    assertNotNull(pdss);
    assertEquals(1,pdss.length);
    assertTrue(pdss[0] instanceof GeneralStanza);
    assertEquals("scion",pdss[0].getName());
    assertEquals("Development",pdss[0].getProperties().get( CabalSyntax.FIELD_CATEGORY ));

    String description="Scion is a Haskell library that aims to implement those parts of a"
      +System.getProperty( "line.separator" )+"Haskell IDE which are independent of a particular front-end.  Scion"
      +System.getProperty( "line.separator" )+"is based on the GHC API and Cabal.  It provides both a Haskell API and"
      +System.getProperty( "line.separator" )+"a server for non-Haskell clients such as Emacs and Vim."
      +System.getProperty( "line.separator" )
      +System.getProperty( "line.separator" )+"See the homepage <http://code.google.com/p/scion-lib> and the README"
      +System.getProperty( "line.separator" )+"<http://github.com/nominolo/scion/blob/master/README.markdown> for"
      +System.getProperty( "line.separator" )+"more information.";
    assertEquals(description,pdss[0].getProperties().get( CabalSyntax.FIELD_DESCRIPTION));
    String newDesc="First line"+System.getProperty( "line.separator" )+System.getProperty( "line.separator" )+"Line2";
    RealValuePosition rvp=pdss[0].update( CabalSyntax.FIELD_DESCRIPTION, newDesc );

    assertEquals(System.getProperty( "line.separator" )+"  First line"+System.getProperty( "line.separator" )+"  ."+System.getProperty( "line.separator" )+"  Line2"+System.getProperty( "line.separator" ),rvp.getRealValue());
    assertEquals(12,rvp.getInitialIndent());
    assertEquals(8,rvp.getStartLine());
    assertEquals(18,rvp.getEndLine());
  }

  public void testList(){
    String content3=getContent( "Example1.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    PackageDescriptionStanza[] pdss=pd.getStanzas();
    PackageDescriptionStanza pds=pdss[1];
    pds.addToPropertyList( CabalSyntax.FIELD_EXPOSED_MODULES , "Test.New" );
    String s=pds.getProperties().get( CabalSyntax.FIELD_EXPOSED_MODULES );
    assertEquals("Test.HUnit.Base, Test.HUnit.Lang, Test.HUnit.Terminal,"+System.getProperty( "line.separator" )+"Test.HUnit.Text, Test.HUnit, Test.New",s);

    pds.removeFromPropertyList( CabalSyntax.FIELD_EXPOSED_MODULES , "Test.HUnit.Base" );
    s=pds.getProperties().get( CabalSyntax.FIELD_EXPOSED_MODULES );
    assertEquals("Test.HUnit.Lang, Test.HUnit.Terminal, "+System.getProperty( "line.separator" )+"Test.HUnit.Text, Test.HUnit, Test.New",s);
  }

  public void testSourceDirs(){
    String content3=getContent( "Source.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    Map<String,List<PackageDescriptionStanza>> map=pd.getStanzasBySourceDir();
    assertEquals(3,map.size());
    assertTrue(map.containsKey( "lib1" ));
    assertTrue(map.containsKey( "prog1" ));
    assertTrue(map.containsKey( "prog2" ));

    List<PackageDescriptionStanza> ls=map.get("lib1");
    assertEquals(3,ls.size());
    assertEquals(CabalSyntax.SECTION_LIBRARY,ls.get( 0 ).getType());
    assertEquals("program1",ls.get( 1 ).getName());
    assertEquals("program2",ls.get( 2 ).getName());

    ls=map.get("prog1");
    assertEquals(1,ls.size());
    assertEquals("program1",ls.get( 0 ).getName());

    ls=map.get("prog2");
    assertEquals(1,ls.size());
    assertEquals("program2",ls.get( 0 ).getName());
  }

  public void testSetNull(){
    String content3=getContent( "Example1.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    PackageDescriptionStanza pds=pd.getStanzas()[1];
    pds.update( CabalSyntax.FIELD_OTHER_MODULES, null );
    assertNull(pds.getProperties().get( CabalSyntax.FIELD_OTHER_MODULES ));
    pds.removeFromPropertyList( CabalSyntax.FIELD_OTHER_MODULES, "Test" );
    assertNull(pds.getProperties().get( CabalSyntax.FIELD_OTHER_MODULES ));

  }
}
