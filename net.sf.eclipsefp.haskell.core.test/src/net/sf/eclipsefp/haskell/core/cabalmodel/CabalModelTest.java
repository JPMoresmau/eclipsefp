package net.sf.eclipsefp.haskell.core.cabalmodel;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
      List<PackageDescriptionStanza> pdss=pd.getStanzas();
      assertNotNull(pdss);
      assertEquals(2,pdss.size());
      assertTrue(pdss.get(0) instanceof GeneralStanza);
      assertEquals(0,pdss.get(0).getIndent());
      assertEquals("HUnit",pdss.get(0).getName());
      assertEquals(0,pdss.get(0).getStartLine());
      assertEquals(9,pdss.get(0).getEndLine());
      assertNotNull(pdss.get(0).getProperties());
      assertEquals(9,pdss.get(0).getProperties().size());
      assertEquals("HUnit",pdss.get(0).getProperties().get( "name"));
      assertEquals("HUnit",pdss.get(0).getProperties().get( "Name"));
      assertEquals("HUnit",pdss.get(0).getProperties().get( CabalSyntax.FIELD_NAME));
      ValuePosition vp=pdss.get(0).getPositions().get(CabalSyntax.FIELD_NAME);
      assertEquals(0,vp.getStartLine());
      assertEquals(1,vp.getEndLine());
      assertEquals(7,vp.getInitialIndent());
      assertEquals(13,vp.getSubsequentIndent());
      assertEquals("1.1.1",pdss.get(0).getProperties().get( "Version"));
      assertEquals(">= 1.2",pdss.get(0).getProperties().get( "Cabal-Version"));
      assertEquals("BSD3",pdss.get(0).getProperties().get( "License"));
      assertEquals("LICENSE",pdss.get(0).getProperties().get( CabalSyntax.FIELD_LICENSE_FILE));
      assertEquals("Dean Herington",pdss.get(0).getProperties().get( "Author"));
      assertEquals("A unit testing framework for Haskell",pdss.get(0).getProperties().get( "Synopsis"));
      assertEquals("http://hunit.sourceforge.net/",pdss.get(0).getProperties().get( "Homepage"));
      assertEquals("Testing",pdss.get(0).getProperties().get( CabalSyntax.FIELD_CATEGORY));

      assertEquals(CabalSyntax.SECTION_LIBRARY,pdss.get(1).getType());
      assertNull(pdss.get(1).getName());
      assertEquals(2,pdss.get(1).getIndent());
      assertEquals(10,pdss.get(1).getStartLine());
      assertEquals(16,pdss.get(1).getEndLine());
      assertNotNull(pdss.get(1).getProperties());
      assertEquals(3,pdss.get(1).getProperties().size());
      assertEquals("base",pdss.get(1).getProperties().get( "Build-Depends"));
      vp=pdss.get(1).getPositions().get("Build-Depends");
      assertEquals(11,vp.getStartLine());
      assertEquals(12,vp.getEndLine());
      assertEquals(17,vp.getInitialIndent());
      assertEquals(20,vp.getSubsequentIndent());
      assertEquals("CPP",pdss.get(1).getProperties().get( "Extensions"));
      assertEquals("Test.HUnit.Base, Test.HUnit.Lang, Test.HUnit.Terminal,"+System.getProperty( "line.separator" )+"Test.HUnit.Text, Test.HUnit",pdss.get(1).getProperties().get( "Exposed-modules"));
  }

  public void testModifyExample1(){
    String content3=getContent( "Small.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    PackageDescriptionStanza pds=pdss.get(0);
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
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    assertNotNull(pdss);
    assertEquals(4,pdss.size());
    assertTrue(pdss.get(0) instanceof GeneralStanza);
    assertEquals("TestPackage",pdss.get(0).getName());
    assertEquals(0,pdss.get(0).getStartLine());
    assertEquals(7,pdss.get(0).getEndLine());
    assertNotNull(pdss.get(0).getProperties());
    assertEquals(7,pdss.get(0).getProperties().size());
    assertEquals("TestPackage",pdss.get(0).getProperties().get( "name"));
    assertEquals("TestPackage",pdss.get(0).getProperties().get( "Name"));
    assertEquals("TestPackage",pdss.get(0).getProperties().get( "NAME"));
    assertEquals("TestPackage",pdss.get(0).getProperties().get( CabalSyntax.FIELD_NAME));
    assertEquals("0.0",pdss.get(0).getProperties().get( "Version"));
    assertEquals(">= 1.2",pdss.get(0).getProperties().get( "Cabal-Version"));
    assertEquals("BSD3",pdss.get(0).getProperties().get( "License"));
    assertEquals("Angela Author",pdss.get(0).getProperties().get( "Author"));
    assertEquals("Package with library and two programs",pdss.get(0).getProperties().get( "Synopsis"));
    assertEquals("Simple",pdss.get(0).getProperties().get( "Build-Type"));

    assertEquals(CabalSyntax.SECTION_LIBRARY,pdss.get(1).getType());
    assertNull(pdss.get(1).getName());
    assertEquals(8,pdss.get(1).getStartLine());
    assertEquals(11,pdss.get(1).getEndLine());
    assertNotNull(pdss.get(1).getProperties());
    assertEquals(2,pdss.get(1).getProperties().size());
    assertEquals("HUnit",pdss.get(1).getProperties().get( "Build-Depends"));
    assertEquals("A, B, C",pdss.get(1).getProperties().get( CabalSyntax.FIELD_EXPOSED_MODULES));

    assertEquals(CabalSyntax.SECTION_EXECUTABLE,pdss.get(2).getType());
    assertEquals("program1",pdss.get(2).getName());
    assertEquals(12,pdss.get(2).getStartLine());
    assertEquals(16,pdss.get(2).getEndLine());
    assertNotNull(pdss.get(2).getProperties());
    assertEquals(3,pdss.get(2).getProperties().size());
    assertEquals("Main.hs",pdss.get(2).getProperties().get( "Main-Is"));
    assertEquals("prog1",pdss.get(2).getProperties().get( "Hs-Source-Dirs"));
    assertEquals("A, B",pdss.get(2).getProperties().get( "Other-Modules"));


    assertEquals(CabalSyntax.SECTION_EXECUTABLE,pdss.get(3).getType());
    assertEquals("program2",pdss.get(3).getName());
    assertEquals(17,pdss.get(3).getStartLine());
    assertEquals(21,pdss.get(3).getEndLine());
    assertNotNull(pdss.get(3).getProperties());
    assertEquals(3,pdss.get(3).getProperties().size());
    assertEquals("Main.hs",pdss.get(3).getProperties().get( "Main-Is"));
    assertEquals("prog2",pdss.get(3).getProperties().get( "Hs-Source-Dirs"));
    assertEquals("A, C, Utils",pdss.get(3).getProperties().get( "Other-Modules"));

  }

  public void testParseSourceRep(){
    String content3=getContent( "SourceRep.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    assertNotNull(pdss);
    assertEquals(3,pdss.size());
    assertTrue(pdss.get(0) instanceof GeneralStanza);
    assertEquals("HUnit",pdss.get(0).getName());

    assertEquals(CabalSyntax.SECTION_SOURCE_REPOSITORY,pdss.get(1).getType());
    assertEquals("head",pdss.get(1).getName());
    assertEquals("darcs",pdss.get(1).getProperties().get("type"));
    assertEquals("http://darcs.haskell.org/cabal/",pdss.get(1).getProperties().get("location"));


    assertEquals(CabalSyntax.SECTION_SOURCE_REPOSITORY,pdss.get(2).getType());
    assertEquals("this",pdss.get(2).getName());
    assertEquals("darcs",pdss.get(2).getProperties().get("type"));
    assertEquals("http://darcs.haskell.org/cabal-branches/cabal-1.6/",pdss.get(2).getProperties().get("location"));
    assertEquals("1.6.1",pdss.get(2).getProperties().get( "tag" ));

  }

  public void testParseExample4(){
    String content3=getContent( "Example4.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    assertNotNull(pdss);
    assertEquals(5,pdss.size());
    assertTrue(pdss.get(0) instanceof GeneralStanza);
    assertEquals("Test1",pdss.get(0).getName());

    assertEquals(CabalSyntax.SECTION_FLAG,pdss.get(1).getType());
    assertEquals("Debug",pdss.get(1).getName());
    assertEquals("Enable debug support",pdss.get(1).getProperties().get( CabalSyntax.FIELD_DESCRIPTION ));
    assertEquals("False",pdss.get(1).getProperties().get( CabalSyntax.FIELD_DEFAULT ));


    assertEquals(CabalSyntax.SECTION_FLAG,pdss.get(2).getType());
    assertEquals("WebFrontend",pdss.get(2).getName());

    assertEquals(CabalSyntax.SECTION_LIBRARY,pdss.get(3).getType());
    assertEquals(17,pdss.get(3).getStartLine());
    assertEquals(32,pdss.get(3).getEndLine());

    List<PackageDescriptionStanza> libraryChildren=pdss.get( 3 ).getStanzas();
    assertEquals(2,libraryChildren.size());
    assertEquals(CabalSyntax.SECTION_IF,libraryChildren.get(0).getType());
    assertEquals(22,libraryChildren.get(0).getStartLine());
    assertEquals(28,libraryChildren.get(0).getEndLine());
    assertEquals(4,libraryChildren.get(0).getIndent());

    assertEquals(2,libraryChildren.get(0).getStanzas().size());
    assertEquals(CabalSyntax.SECTION_IF,libraryChildren.get(0).getStanzas().get(0).getType());
    assertEquals(24,libraryChildren.get(0).getStanzas().get(0).getStartLine());
    assertEquals(26,libraryChildren.get(0).getStanzas().get(0).getEndLine());
    assertEquals(6,libraryChildren.get(0).getStanzas().get(0).getIndent());
    assertEquals(CabalSyntax.SECTION_ELSE,libraryChildren.get(0).getStanzas().get(1).getType());
    assertEquals(26,libraryChildren.get(0).getStanzas().get(1).getStartLine());
    assertEquals(28,libraryChildren.get(0).getStanzas().get(1).getEndLine());
    assertEquals(6,libraryChildren.get(0).getStanzas().get(1).getIndent());


    assertEquals(CabalSyntax.SECTION_IF,libraryChildren.get(1).getType());
    assertEquals(29,libraryChildren.get(1).getStartLine());
    assertEquals(32,libraryChildren.get(1).getEndLine());
    assertEquals(4,libraryChildren.get(1).getIndent());

    //assertEquals(CabalSyntax.SECTION_IF,pdss.get(4).getType());

    //assertEquals(CabalSyntax.SECTION_IF,pdss.get(5).getType());

    assertEquals(CabalSyntax.SECTION_EXECUTABLE,pdss.get(4).getType());

    //assertEquals(CabalSyntax.SECTION_IF,pdss.get(7).getType());
  }

  public void testParseExample5(){
    String content3=getContent( "Example5.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    assertNotNull(pdss);
    assertEquals(3,pdss.size());
    assertTrue(pdss.get(0) instanceof GeneralStanza);
    assertEquals("Test1",pdss.get(0).getName());

    assertEquals(CabalSyntax.SECTION_FLAG,pdss.get(1).getType());
    assertEquals("Debug",pdss.get(1).getName());
    assertEquals("Enable debug support",pdss.get(1).getProperties().get( CabalSyntax.FIELD_DESCRIPTION ));
    assertEquals("False",pdss.get(1).getProperties().get( CabalSyntax.FIELD_DEFAULT ));


    assertEquals(CabalSyntax.SECTION_LIBRARY,pdss.get(2).getType());
    assertEquals("Testing.Test1",pdss.get(2).getProperties().get( CabalSyntax.FIELD_EXPOSED_MODULES ));

    assertEquals(1,pdss.get(2).getStanzas().size());
    PackageDescriptionStanza pds1=pdss.get( 2 ).getStanzas().get( 0 );
    assertEquals("-DDEBUG",pds1.getProperties().get( CabalSyntax.FIELD_GHC_OPTIONS ));

    assertEquals(2,pds1.getStanzas().size());

    assertEquals("\"-DDEBUG\"",pds1.getStanzas().get(0).getProperties().get( CabalSyntax.FIELD_CC_OPTIONS ));
    assertEquals("\"-DNDEBUG\"",pds1.getStanzas().get(1).getProperties().get( CabalSyntax.FIELD_CC_OPTIONS ));

  }

  public void testSpaces(){
    String content3=getContent( "Spaces.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    assertNotNull(pdss);
    assertEquals(1,pdss.size());
    assertTrue(pdss.get(0) instanceof GeneralStanza);
    assertEquals("scion",pdss.get(0).getName());
    assertEquals("Development",pdss.get(0).getProperties().get( CabalSyntax.FIELD_CATEGORY ));

    String description="Scion is a Haskell library that aims to implement those parts of a"
      +System.getProperty( "line.separator" )+"Haskell IDE which are independent of a particular front-end.  Scion"
      +System.getProperty( "line.separator" )+"is based on the GHC API and Cabal.  It provides both a Haskell API and"
      +System.getProperty( "line.separator" )+"a server for non-Haskell clients such as Emacs and Vim."
      +System.getProperty( "line.separator" )
      +System.getProperty( "line.separator" )+"See the homepage <http://code.google.com/p/scion-lib> and the README"
      +System.getProperty( "line.separator" )+"<http://github.com/nominolo/scion/blob/master/README.markdown> for"
      +System.getProperty( "line.separator" )+"more information.";
    assertEquals(description,pdss.get(0).getProperties().get( CabalSyntax.FIELD_DESCRIPTION));
    ValuePosition vp=pdss.get(0).getPositions().get(CabalSyntax.FIELD_DESCRIPTION );
    assertEquals(12,vp.getInitialIndent());
    assertEquals(8,vp.getStartLine());
    assertEquals(17,vp.getEndLine());

    String newDesc="First line"+System.getProperty( "line.separator" )+System.getProperty( "line.separator" )+"Line2";
    RealValuePosition rvp=pdss.get(0).update( CabalSyntax.FIELD_DESCRIPTION, newDesc );

    assertEquals(System.getProperty( "line.separator" )+"  First line"+System.getProperty( "line.separator" )+"  ."+System.getProperty( "line.separator" )+"  Line2"+System.getProperty( "line.separator" ),rvp.getRealValue());
    assertEquals(12,rvp.getInitialIndent());
    assertEquals(8,rvp.getStartLine());
    assertEquals(17,rvp.getEndLine());
  }

  public void testList(){
    String content3=getContent( "Example1.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    PackageDescriptionStanza pds=pdss.get(1);
    pds.addToPropertyList( CabalSyntax.FIELD_EXPOSED_MODULES , "Test.New" );
    String s=pds.getProperties().get( CabalSyntax.FIELD_EXPOSED_MODULES );
    assertEquals("Test.HUnit.Base, Test.HUnit.Lang, Test.HUnit.Terminal,"+System.getProperty( "line.separator" )+"Test.HUnit.Text, Test.HUnit, Test.New",s);

    pds.removeFromPropertyList( CabalSyntax.FIELD_EXPOSED_MODULES , "Test.HUnit.Base" );
    s=pds.getProperties().get( CabalSyntax.FIELD_EXPOSED_MODULES );
    assertEquals("Test.HUnit.Lang, Test.HUnit.Terminal, Test.HUnit.Text, Test.HUnit, Test.New",s);
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
    PackageDescriptionStanza pds=pd.getStanzas().get(1);
    pds.update( CabalSyntax.FIELD_OTHER_MODULES, null );
    assertNull(pds.getProperties().get( CabalSyntax.FIELD_OTHER_MODULES ));
    pds.removeFromPropertyList( CabalSyntax.FIELD_OTHER_MODULES, "Test" );
    assertNull(pds.getProperties().get( CabalSyntax.FIELD_OTHER_MODULES ));

  }

  public void testScion(){
    String content3=getContent( "scion.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );

    PackageDescriptionStanza pds=pd.getStanzas().get( 4 );
    assertEquals(CabalSyntax.SECTION_EXECUTABLE,pds.getType());

    String initial="Scion"
      +System.getProperty( "line.separator" )+"Scion.Cabal"
      +System.getProperty( "line.separator" )+"Scion.Inspect"
      +System.getProperty( "line.separator" )+"Scion.Inspect.DefinitionSite"
      +System.getProperty( "line.separator" )+"Scion.Session"
      +System.getProperty( "line.separator" )+"Scion.Types"
      +System.getProperty( "line.separator" )+"Scion.Types.Notes"
      +System.getProperty( "line.separator" )+"Scion.Utils"

      +System.getProperty( "line.separator" )+"Scion.Server.Commands"
      +System.getProperty( "line.separator" )+"Scion.Server.ConnectionIO"
      +System.getProperty( "line.separator" )+"Scion.Server.Generic"
      +System.getProperty( "line.separator" )+"Scion.Server.Protocol";
    assertEquals(initial,pds.getProperties().get( CabalSyntax.FIELD_OTHER_MODULES ));

    ValuePosition vp=pds.getPositions().get( CabalSyntax.FIELD_OTHER_MODULES );
    assertEquals(122,vp.getStartLine());
    assertEquals(136,vp.getEndLine());
    assertEquals(16,vp.getInitialIndent());
    assertEquals(4,vp.getSubsequentIndent());

    RealValuePosition rvp=pds.addToPropertyList( CabalSyntax.FIELD_OTHER_MODULES, "Scion.Test" );
    assertEquals(122,rvp.getStartLine());
    assertEquals(136,rvp.getEndLine());
    assertEquals(16,rvp.getInitialIndent());
    //assertEquals(4,rvp.getSubsequentIndent());
    assertEquals(System.getProperty( "line.separator" )+"    "+initial.replaceAll( "\\n", "\n    " )+", Scion.Test"+System.getProperty( "line.separator" ),rvp.getRealValue());

    rvp=pds.addToPropertyList( CabalSyntax.FIELD_OTHER_MODULES, "Scion.Test" );
    assertEquals(122,rvp.getStartLine());
    assertEquals(134,rvp.getEndLine());
    assertEquals(16,rvp.getInitialIndent());
    //assertEquals(4,rvp.getSubsequentIndent());
    assertEquals(System.getProperty( "line.separator" )+"    "+initial.replaceAll( "\\n", "\n    " )+", Scion.Test"+System.getProperty( "line.separator" ),rvp.getRealValue());

    rvp=pds.removeFromPropertyList( CabalSyntax.FIELD_OTHER_MODULES, "Scion.Test" );
    assertEquals(122,rvp.getStartLine());
    assertEquals(134,rvp.getEndLine());
    assertEquals(16,rvp.getInitialIndent());
    assertEquals(initial.replaceAll( "\\r\\n", ", " ).replaceAll( "\\n", ", " )+System.getProperty( "line.separator" ),rvp.getRealValue());

  }

  public void testDependantPackages(){
    String content3=getContent( "scion.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    PackageDescriptionStanza pds=pd.getStanzas().get( 3 );
    assertEquals(CabalSyntax.SECTION_LIBRARY,pds.getType());
    Collection<String> ss=pds.getDependentPackages();
    Set<String> expected=new HashSet<String>();
    expected.addAll( Arrays.asList( "base"  ,
        "Cabal",
        "containers",
        "directory",
        "filepath",
        "ghc",
        "ghc-paths",
        "ghc-syb",
        "hslogger",
        "json",
        "multiset",
        "time",
        "uniplate",
        "list-tries",
        "binary",
        "array" ) );

    assertEquals(expected,ss);

    expected.remove("uniplate");
    pds=pd.getStanzas().get( 4 );
    assertEquals(CabalSyntax.SECTION_EXECUTABLE,pds.getType());
    ss=pds.getDependentPackages();
    assertEquals(expected,ss);
  }

  public void testCreate(){
    PackageDescription pd=PackageDescriptionLoader.load( "Name: newProject"+System.getProperty( "line.separator" ) );
    PackageDescriptionStanza pds=pd.getStanzas().get(0);
    RealValuePosition rvp=pds.update( CabalSyntax.FIELD_AUTHOR , "JP Moresmau" );
    assertNotNull(rvp);
    assertEquals(1,rvp.getStartLine());
    assertEquals(1,rvp.getEndLine());
    assertEquals(CabalSyntax.FIELD_AUTHOR.toString()+": JP Moresmau"+System.getProperty( "line.separator" ),rvp.getRealValue());
  }

}
