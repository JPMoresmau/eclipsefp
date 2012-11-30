package net.sf.eclipsefp.haskell.core.cabalmodel;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.PlatformUtil;


public class CabalModelTest extends TestCase {

  public CabalModelTest( final String name ) {
    super( name );
  }

  public static TestSuite suite(){
    TestSuite ts=new TestSuite("CabalModelTest");
    //ts.addTest( new CabalModelTest( "testJSON" ) );
    ts.addTestSuite( CabalModelTest.class );
    return ts;
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();

  }

  public static String getContent(final String fileName){
    try {
      InputStream is=CabalModelTest.class.getResourceAsStream( fileName);
      ByteArrayOutputStream baos=new ByteArrayOutputStream();
      int c=-1;
      while ((c=is.read())!=-1){
        baos.write(c);
      }
      is.close();
      return new String(baos.toByteArray(),FileUtil.UTF8);
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
      assertTrue(pdss.get(0) instanceof PackagePropertiesStanza);
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
      assertEquals(17,pdss.get(1).getEndLine());
      assertNotNull(pdss.get(1).getProperties());
      assertEquals(3,pdss.get(1).getProperties().size());
      assertEquals("multiset     >= 0.1 && < 0.3,"+PlatformUtil.NL+"base",pdss.get(1).getProperties().get( CabalSyntax.FIELD_BUILD_DEPENDS));
      vp=pdss.get(1).getPositions().get(CabalSyntax.FIELD_BUILD_DEPENDS);
      assertEquals(11,vp.getStartLine());
      assertEquals(13,vp.getEndLine());
      assertEquals(18,vp.getInitialIndent());
      assertEquals(4,vp.getSubsequentIndent());
      assertEquals("CPP",pdss.get(1).getProperties().get( "Extensions"));
      assertEquals("Test.HUnit.Base, Test.HUnit.Lang, Test.HUnit.Terminal,"+PlatformUtil.NL+"Test.HUnit.Text, Test.HUnit",pdss.get(1).getProperties().get( "Exposed-modules"));
  }

  public void testModifySmall(){
    String content3=getContent( "Small.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    PackageDescriptionStanza pds=pdss.get(0);
    assertEquals("HUnit",pds.getProperties().get( CabalSyntax.FIELD_NAME ));
    RealValuePosition rvp=pds.update( CabalSyntax.FIELD_NAME, "JP" );
    assertEquals("JP"+PlatformUtil.NL,rvp.getRealValue());
    assertEquals(0,rvp.getStartLine());
    assertEquals(1,rvp.getEndLine());
    assertEquals(13,rvp.getInitialIndent());
    rvp=pds.update( CabalSyntax.FIELD_NAME, "JP2" );
    assertEquals("JP2"+PlatformUtil.NL,rvp.getRealValue());
    assertEquals(0,rvp.getStartLine());
    assertEquals(1,rvp.getEndLine());
    assertEquals(13,rvp.getInitialIndent());

    rvp=pds.update( CabalSyntax.FIELD_VERSION, "1.0" );
    assertEquals(CabalSyntax.FIELD_VERSION+":     1.0"+PlatformUtil.NL,rvp.getRealValue());
    assertEquals(1,rvp.getStartLine());
    assertEquals(1,rvp.getEndLine());
    assertEquals(0,rvp.getInitialIndent());

    rvp=pds.update( CabalSyntax.FIELD_VERSION, "1.1.1" );
    assertEquals("1.1.1"+PlatformUtil.NL,rvp.getRealValue());
    assertEquals(1,rvp.getStartLine());
    assertEquals(2,rvp.getEndLine());
    assertEquals(13,rvp.getInitialIndent());



  }

  public void testModifyExample1(){
    String content3=getContent( "Example1.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    PackageDescriptionStanza pds=pdss.get(1);
    RealValuePosition rvp=pds.removePrefixFromPropertyList( CabalSyntax.FIELD_BUILD_DEPENDS, "multiset", "," );
    assertEquals("base"+PlatformUtil.NL,rvp.getRealValue());

    pd=PackageDescriptionLoader.load( content3 );
    pdss=pd.getStanzas();
    pds=pdss.get(1);
    rvp=pds.removePrefixFromPropertyList( CabalSyntax.FIELD_BUILD_DEPENDS, "base", "," );
    assertEquals("multiset     >= 0.1 && < 0.3"+PlatformUtil.NL,rvp.getRealValue());
  }

  public void testParseExample3(){
    String content3=getContent( "Example3.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    assertNotNull(pdss);
    assertEquals(4,pdss.size());
    assertTrue(pdss.get(0) instanceof PackagePropertiesStanza);
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

  public void testParseExample3Tab(){
    String content3=getContent( "Example3.cabal" );
    content3=content3.replace( "Executable program1", "Executable\tprogram1" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    assertNotNull(pdss);
    assertEquals(4,pdss.size());
    assertTrue(pdss.get(0) instanceof PackagePropertiesStanza);
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
    assertTrue(pdss.get(0) instanceof PackagePropertiesStanza);
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
    assertTrue(pdss.get(0) instanceof PackagePropertiesStanza);
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
    assertTrue(pdss.get(0) instanceof PackagePropertiesStanza);
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
    assertTrue(pdss.get(0) instanceof PackagePropertiesStanza);
    assertEquals("scion",pdss.get(0).getName());
    assertEquals("Development",pdss.get(0).getProperties().get( CabalSyntax.FIELD_CATEGORY ));

    String description="Scion is a Haskell library that aims to implement those parts of a"
      +PlatformUtil.NL+"Haskell IDE which are independent of a particular front-end.  Scion"
      +PlatformUtil.NL+"is based on the GHC API and Cabal.  It provides both a Haskell API and"
      +PlatformUtil.NL+"a server for non-Haskell clients such as Emacs and Vim."
      +PlatformUtil.NL
      +PlatformUtil.NL+"See the homepage <http://code.google.com/p/scion-lib> and the README"
      +PlatformUtil.NL+"<http://github.com/nominolo/scion/blob/master/README.markdown> for"
      +PlatformUtil.NL+"more information.";
    assertEquals(description,pdss.get(0).getProperties().get( CabalSyntax.FIELD_DESCRIPTION));
    ValuePosition vp=pdss.get(0).getPositions().get(CabalSyntax.FIELD_DESCRIPTION );
    assertEquals(12,vp.getInitialIndent());
    assertEquals(8,vp.getStartLine());
    assertEquals(17,vp.getEndLine());

    String newDesc="First line"+PlatformUtil.NL+PlatformUtil.NL+"Line2";
    RealValuePosition rvp=pdss.get(0).update( CabalSyntax.FIELD_DESCRIPTION, newDesc );

    assertEquals(PlatformUtil.NL+"  First line"+PlatformUtil.NL+"  ."+PlatformUtil.NL+"  Line2"+PlatformUtil.NL,rvp.getRealValue());
    assertEquals(12,rvp.getInitialIndent());
    assertEquals(8,rvp.getStartLine());
    assertEquals(17,rvp.getEndLine());

    String s=pd.dump();
    System.out.println(s);
    assertTrue(s.contains( "  First line"+PlatformUtil.NL+"  ."+PlatformUtil.NL+"  Line2" ) );
    pd=PackageDescriptionLoader.load( s );

    String data="docs/doc1.txt, "+PlatformUtil.NL+"docs/doc2.txt";
    rvp=pdss.get(0).update( CabalSyntax.FIELD_DATA_FILES, data );
    assertEquals(PlatformUtil.NL+CabalSyntax.FIELD_DATA_FILES+":      "+PlatformUtil.NL+"                 docs/doc1.txt, "+PlatformUtil.NL+"                 docs/doc2.txt"+PlatformUtil.NL,rvp.getRealValue());

  }

  public void testList(){
    String content3=getContent( "Example1.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    PackageDescriptionStanza pds=pdss.get(1);
    pds.addToPropertyList( CabalSyntax.FIELD_EXPOSED_MODULES , "Test.New" );
    String s=pds.getProperties().get( CabalSyntax.FIELD_EXPOSED_MODULES );
    assertEquals("Test.HUnit.Base, Test.HUnit.Lang, Test.HUnit.Terminal,"+PlatformUtil.NL+"Test.HUnit.Text, Test.HUnit,"+PlatformUtil.NL+"Test.New",s);

    pds.removeFromPropertyList( CabalSyntax.FIELD_EXPOSED_MODULES , "Test.HUnit.Base" );
    s=pds.getProperties().get( CabalSyntax.FIELD_EXPOSED_MODULES );
    assertEquals("Test.HUnit.Lang,"+PlatformUtil.NL+"Test.HUnit.Terminal,"+PlatformUtil.NL+"Test.HUnit.Text,"+PlatformUtil.NL+"Test.HUnit,"+PlatformUtil.NL+"Test.New",s);

    pds.update( CabalSyntax.FIELD_EXPOSED_MODULES, null );
    s=pds.getProperties().get( CabalSyntax.FIELD_EXPOSED_MODULES );
    assertNull(s);
    pds.addToPropertyList( CabalSyntax.FIELD_EXPOSED_MODULES , "Test.New" );
    s=pds.getProperties().get( CabalSyntax.FIELD_EXPOSED_MODULES );
    assertEquals("Test.New",s);

    pds.update(  CabalSyntax.FIELD_EXPOSED_MODULES,"Test.New,");
    pds.addToPropertyList( CabalSyntax.FIELD_EXPOSED_MODULES , "Test.New2" );
    s=pds.getProperties().get( CabalSyntax.FIELD_EXPOSED_MODULES );
    assertEquals("Test.New,"+PlatformUtil.NL+"Test.New2",s);

    pds.update(  CabalSyntax.FIELD_EXPOSED_MODULES,"Test.New, ");
    pds.addToPropertyList( CabalSyntax.FIELD_EXPOSED_MODULES , "Test.New2" );
    s=pds.getProperties().get( CabalSyntax.FIELD_EXPOSED_MODULES );
    assertEquals("Test.New, "+PlatformUtil.NL+"Test.New2",s);

    pds.update(  CabalSyntax.FIELD_EXPOSED_MODULES,"Test.New,"+PlatformUtil.NL);
    pds.addToPropertyList( CabalSyntax.FIELD_EXPOSED_MODULES , "Test.New2" );
    s=pds.getProperties().get( CabalSyntax.FIELD_EXPOSED_MODULES );
    assertEquals("Test.New,"+PlatformUtil.NL+"Test.New2",s);
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

  public void testEmptySourceDir(){
    String content3=getContent( "Example1.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    Map<String,List<PackageDescriptionStanza>> map=pd.getStanzasBySourceDir();
    assertEquals(1,map.size());
    assertTrue(map.containsKey( "." ));

    List<PackageDescriptionStanza> ls=map.get(".");
    assertEquals(1,ls.size());
    assertEquals(CabalSyntax.SECTION_LIBRARY,ls.get( 0 ).getType());
    Collection<String> cs=ls.get( 0 ).getSourceDirs();
    assertEquals(1,cs.size());
    assertEquals(".",cs.iterator().next());

    assertEquals(0,pd.getStanzas().get( 0 ).getSourceDirs().size());

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

    Map<String,List<PackageDescriptionStanza>> m=pd.getStanzasBySourceDir();
    assertEquals(2,m.size());
    List<PackageDescriptionStanza> pdLib=m.get( "lib" );
    assertNotNull(pdLib );
    assertEquals(2,pdLib.size());
    assertTrue(pdLib.contains(pd.getStanzas().get( 3 )));
    assertTrue(pdLib.contains(pd.getStanzas().get( 4 )));
    List<PackageDescriptionStanza> pdServer=m.get( "server" );
    assertNotNull(pdServer );
    assertEquals(1,pdServer.size());
    assertTrue(pdServer.contains(pd.getStanzas().get( 4 )));

    PackageDescriptionStanza pds=pd.getStanzas().get( 4 );
    assertEquals(CabalSyntax.SECTION_EXECUTABLE,pds.getType());

    String initial="Scion"
      +PlatformUtil.NL+"Scion.Cabal"
      +PlatformUtil.NL+"Scion.Inspect"
      +PlatformUtil.NL+"Scion.Inspect.DefinitionSite"
      +PlatformUtil.NL+"Scion.Session"
      +PlatformUtil.NL+"Scion.Types"
      +PlatformUtil.NL+"Scion.Types.Notes"
      +PlatformUtil.NL+"Scion.Utils"

      +PlatformUtil.NL+"Scion.Server.Commands"
      +PlatformUtil.NL+"Scion.Server.ConnectionIO"
      +PlatformUtil.NL+"Scion.Server.Generic"
      +PlatformUtil.NL+"Scion.Server.Protocol";
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
    assertEquals(PlatformUtil.NL+"    "+initial.replaceAll( "\\n", "\n    " )+","+PlatformUtil.NL+"    Scion.Test"+PlatformUtil.NL,rvp.getRealValue());

    rvp=pds.addToPropertyList( CabalSyntax.FIELD_OTHER_MODULES, "Scion.Test" );
    assertNull(rvp);
//    assertEquals(122,rvp.getStartLine());
//    assertEquals(134,rvp.getEndLine());
//    assertEquals(16,rvp.getInitialIndent());
//    //assertEquals(4,rvp.getSubsequentIndent());
//    assertEquals(PlatformUtil.NL+"    "+initial.replaceAll( "\\n", "\n    " )+", Scion.Test"+PlatformUtil.NL,rvp.getRealValue());

    rvp=pds.removeFromPropertyList( CabalSyntax.FIELD_OTHER_MODULES, "Scion.Test" );
    assertEquals(122,rvp.getStartLine());
    assertEquals(136,rvp.getEndLine());
    assertEquals(16,rvp.getInitialIndent());
    assertEquals(PlatformUtil.NL+"    "+initial.replaceAll( "\\r\\n", ",\r\n    " )+PlatformUtil.NL,rvp.getRealValue());

    String bd=pds.getProperties().get( CabalSyntax.FIELD_BUILD_DEPENDS );
    assertNotNull(bd);
    assertTrue(bd.contains( "multiset     == 0.1.*" ));
    rvp=pds.removePrefixFromPropertyList( CabalSyntax.FIELD_BUILD_DEPENDS, "multiset" ,",");
    assertFalse(rvp.getRealValue().contains("multiset") );
    assertEquals(96,rvp.getStartLine());
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

  public void testCreateField(){
    PackageDescription pd=PackageDescriptionLoader.load( "Name: newProject"+PlatformUtil.NL );
    PackageDescriptionStanza pds=pd.getStanzas().get(0);
    RealValuePosition rvp=pds.update( CabalSyntax.FIELD_AUTHOR , "JP Moresmau" );
    assertNotNull(rvp);
    assertEquals(1,rvp.getStartLine());
    assertEquals(1,rvp.getEndLine());
    assertEquals(CabalSyntax.FIELD_AUTHOR.toString()+": JP Moresmau"+PlatformUtil.NL,rvp.getRealValue());
  }

  public void testCreateFieldNoNL(){
    PackageDescription pd=PackageDescriptionLoader.load( "Name: newProject" );
    PackageDescriptionStanza pds=pd.getStanzas().get(0);
    RealValuePosition rvp=pds.update( CabalSyntax.FIELD_AUTHOR , "JP Moresmau" );
    assertNotNull(rvp);
    assertEquals(0,rvp.getStartLine());
    assertEquals(1,rvp.getEndLine());
    assertEquals(16,rvp.getInitialIndent());
    assertEquals(-1,rvp.getSubsequentIndent());
    assertEquals(PlatformUtil.NL+CabalSyntax.FIELD_AUTHOR.toString()+": JP Moresmau"+PlatformUtil.NL,rvp.getRealValue());
  }

  public void testCreateFieldNoNLAfterMultipleLines(){
    PackageDescription pd=PackageDescriptionLoader.load( "Name: newProject"+PlatformUtil.NL+"Synopsis: firstline"+PlatformUtil.NL+"  secondline" );
    PackageDescriptionStanza pds=pd.getStanzas().get(0);
    RealValuePosition rvp=pds.update( CabalSyntax.FIELD_AUTHOR , "JP Moresmau" );
    assertNotNull(rvp);
    assertEquals(2,rvp.getStartLine());
    assertEquals(3,rvp.getEndLine());
    assertEquals(12,rvp.getInitialIndent());
    assertEquals(-1,rvp.getSubsequentIndent());
    assertEquals(PlatformUtil.NL+CabalSyntax.FIELD_AUTHOR.toString()+": JP Moresmau"+PlatformUtil.NL,rvp.getRealValue());
  }

  public void testCreateFieldFromEmpty(){
    PackageDescription pd=PackageDescriptionLoader.load( "Name: newProject"+PlatformUtil.NL+"Author: "+PlatformUtil.NL+"Summary: Summ"+PlatformUtil.NL );
    PackageDescriptionStanza pds=pd.getStanzas().get(0);
    RealValuePosition rvp=pds.update( CabalSyntax.FIELD_AUTHOR , "JP Moresmau" );
    assertNotNull(rvp);
    assertEquals(1,rvp.getStartLine());
    assertEquals(2,rvp.getEndLine());
    assertEquals(8,rvp.getInitialIndent());
    assertEquals(-1,rvp.getSubsequentIndent());
    assertEquals("JP Moresmau"+PlatformUtil.NL,rvp.getRealValue());
  }

  public void testCreateFromScratch(){
    PackageDescription pd=new PackageDescription( "newProject");
    PackageDescriptionStanza pds=pd.getStanzas().get(0);
    assertEquals("newProject",pds.getName());
    assertEquals(0,pds.getStartLine());
    assertEquals(1,pds.getEndLine());
    RealValuePosition rvp=pds.update( CabalSyntax.FIELD_AUTHOR , "JP Moresmau" );
    assertNotNull(rvp);
    assertEquals(1,rvp.getStartLine());
    assertEquals(1,rvp.getEndLine());
    assertEquals(CabalSyntax.FIELD_AUTHOR.toString()+":  JP Moresmau"+PlatformUtil.NL,rvp.getRealValue());
    assertEquals(0,pds.getStartLine());
    assertEquals(2,pds.getEndLine());
    pds=pd.addStanza( CabalSyntax.SECTION_LIBRARY, null );
    pds.update( CabalSyntax.FIELD_EXPOSED_MODULES, "Mod1" );
    pds.update( CabalSyntax.FIELD_HS_SOURCE_DIRS, FileUtil.DEFAULT_FOLDER_SRC );

    pds=pd.addStanza( CabalSyntax.SECTION_EXECUTABLE, "exe" );
    pds.update( CabalSyntax.FIELD_MAIN_IS, "Main.hs" );
    pds.update( CabalSyntax.FIELD_HS_SOURCE_DIRS, "src, exe" );

    StringWriter sw=new StringWriter();
    try {
      pd.dump( sw );
      String s=sw.toString();
      //System.out.println(s);
      PackageDescription pd2=PackageDescriptionLoader.load( s );
      assertEquals(3,pd2.getStanzas().size());
      pds=pd.getStanzas().get(0);
      assertEquals("newProject",pds.getName());
      assertEquals("JP Moresmau",pds.getProperties().get( CabalSyntax.FIELD_AUTHOR ));

      pds=pd.getStanzas().get(1);
      assertNull(pds.getName());
      assertEquals(CabalSyntax.SECTION_LIBRARY,pds.getType());
      assertEquals("Mod1",pds.getProperties().get( CabalSyntax.FIELD_EXPOSED_MODULES ));
      assertEquals(FileUtil.DEFAULT_FOLDER_SRC,pds.getProperties().get( CabalSyntax.FIELD_HS_SOURCE_DIRS ));

      pds=pd.getStanzas().get(2);
      assertEquals("exe",pds.getName());
      assertEquals(CabalSyntax.SECTION_EXECUTABLE,pds.getType());
      assertEquals("Main.hs",pds.getProperties().get( CabalSyntax.FIELD_MAIN_IS ));
      assertEquals("src, exe",pds.getProperties().get( CabalSyntax.FIELD_HS_SOURCE_DIRS ));

    } catch (IOException ioe){
      ioe.printStackTrace();
      fail(ioe.getLocalizedMessage());
    }
  }

  public void testJSON(){
    String content3=getContent( "json.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    PackageDescriptionStanza pds=pd.getStanzas().get( 0 );
    assertNull(pds.getType());
    assertTrue(pds instanceof PackagePropertiesStanza);
    assertEquals(0,pds.getIndent());
    assertEquals("json",pds.getName());

    pds=pd.getStanzas().get( 1 );
    assertEquals(CabalSyntax.SECTION_FLAG,pds.getType());
    assertEquals("split-base",pds.getName());

    pds=pd.getStanzas().get( 6 );
    assertEquals(CabalSyntax.SECTION_LIBRARY,pds.getType());
    assertEquals(3,pds.getStanzas().size());
    PackageDescriptionStanza pds1=pds.getStanzas().get(0 );
    assertEquals("flag(split-base)",pds1.getName());
    PackageDescriptionStanza pds2=pds1.getStanzas().get(1 );
    assertNull(pds2.getName());

    pds1=pds.getStanzas().get(1 );
    assertNull(pds1.getName());
  }

  public void testModifyBuildDepends(){
    String content3=getContent( "P1.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );

    PackageDescriptionStanza pds1=pd.getStanzas().get( 1 );
    assertNull(pds1.getName());
    RealValuePosition rvp=pds1.addToPropertyList( CabalSyntax.FIELD_BUILD_DEPENDS, "base" );
    assertEquals("  build-depends:   base"+PlatformUtil.NL,rvp.getRealValue());
    assertEquals(8,rvp.getStartLine());
    assertEquals(8,rvp.getEndLine());


    PackageDescriptionStanza pds2=pd.getStanzas().get( 2 );
    assertEquals("P1",pds2.getName());
    rvp=pds2.removePrefixFromPropertyList( CabalSyntax.FIELD_BUILD_DEPENDS, "array","," );
    assertEquals(14,rvp.getStartLine());
    assertEquals(15,rvp.getEndLine());
    assertEquals(0,rvp.getInitialIndent());
  }

  public void testModuleInclusionType(){
    String content3=getContent( "Example4.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    PackageDescriptionStanza lib=pdss.get( 3 );
    assertEquals(ModuleInclusionType.EXPOSED,lib.getModuleInclusionType( "Testing.Test1" ));
    assertEquals(ModuleInclusionType.INCLUDED,lib.getModuleInclusionType( "Testing.WebStuff" ));
    assertEquals(ModuleInclusionType.MISSING,lib.getModuleInclusionType( "T1" ));
    PackageDescriptionStanza exe=pdss.get( 4 );
    assertEquals(ModuleInclusionType.INCLUDED,exe.getModuleInclusionType( "Testing.Test1" ));
    assertEquals(ModuleInclusionType.MAIN,exe.getModuleInclusionType( "T1" ));
    assertEquals(ModuleInclusionType.MISSING,exe.getModuleInclusionType( "Testing.Test2" ));
  }

  public void testTestSuite(){
    String content3=getContent( "TestSuite.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    PackageDescriptionStanza ts=pdss.get( 2 );
    assertEquals(CabalSyntax.SECTION_TESTSUITE,ts.getType());
    assertEquals("exitcode-stdio-1.0",ts.getProperties().get( CabalSyntax.FIELD_TYPE ));
  }

  public void testTestSuitePartial(){
    String content3=getContent( "TestSuiteOnly.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    PackageDescriptionStanza ts=pdss.get( 1 );
    assertEquals(CabalSyntax.SECTION_TESTSUITE,ts.getType());
    assertEquals("exitcode-stdio-1.0",ts.getProperties().get( CabalSyntax.FIELD_TYPE ));
    ts=PackageDescriptionLoader.loadStanza( content3 );
    assertEquals(CabalSyntax.SECTION_TESTSUITE,ts.getType());
    assertEquals("exitcode-stdio-1.0",ts.getProperties().get( CabalSyntax.FIELD_TYPE ));

  }

  public void testIfElse(){
    String content3=getContent( "IfElse.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    assertEquals(3,pdss.size());

    assertEquals(CabalSyntax.SECTION_LIBRARY,pdss.get(1).getType());
    assertNull(pdss.get(1).getName());
    assertEquals(2,pdss.get(1).getIndent());

    assertEquals(CabalSyntax.SECTION_EXECUTABLE,pdss.get(2).getType());
    assertEquals("ifelse",pdss.get(2).getName());
    assertEquals(2,pdss.get(2).getIndent());
  }

  public void testRemove(){
    String content3=getContent( "P1.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    assertEquals(4,pdss.size());
    assertEquals(CabalSyntax.SECTION_EXECUTABLE,pdss.get(2).getType());
    assertEquals("P1",pdss.get(2).getName());
    assertEquals(2,pdss.get(2).getIndent());
    assertEquals(5,pdss.get(2).getProperties().size());
    RealValuePosition vp = pdss.get(2).update( CabalSyntax.FIELD_MAIN_IS, "" );
    assertEquals("",vp.getRealValue());
    assertEquals(0,vp.getInitialIndent());
    assertEquals(11,vp.getStartLine());
    assertEquals(12,vp.getEndLine());
    assertEquals(4,pdss.get(2).getProperties().size());
    TestDocument doc=new TestDocument(content3);
    //System.out.println(doc.get());
    vp.updateDocument(doc );
    //System.out.println(doc.get());
    pd=PackageDescriptionLoader.load( doc.get() );
    pdss=pd.getStanzas();
    assertEquals(4,pdss.size());
    assertEquals(CabalSyntax.SECTION_EXECUTABLE,pdss.get(2).getType());
    assertEquals("P1",pdss.get(2).getName());
    assertEquals(2,pdss.get(2).getIndent());
    assertEquals(4,pdss.get(2).getProperties().size());
  }

  public void testMultipleUpdate() throws IOException{
    String content3=getContent( "P1.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    assertEquals(4,pdss.size());
    PackageDescriptionStanza pds=pdss.get(2);
    pds.update( CabalSyntax.FIELD_EXTENSIONS, "UndecidableInstances,"+PlatformUtil.NL+"OverlappingInstances" );
    pds.update( CabalSyntax.FIELD_BUILD_DEPENDS,"array,"+PlatformUtil.NL+"random");
    StringWriter sw=new StringWriter();
    pd.dump(sw );
    sw.flush();
    String s=sw.toString();
    //System.out.println(s);
    pd=PackageDescriptionLoader.load( s );
    pdss=pd.getStanzas();
    assertEquals(4,pdss.size());
    pds=pdss.get(2);
    String val=pds.getProperties().get(CabalSyntax.FIELD_BUILD_DEPENDS);
    List<String> ls=PackageDescriptionLoader.parseList( val );
    assertEquals(2,ls.size());
    assertTrue(ls.contains( "array" ));
    assertTrue(ls.contains( "random" ));
    val=pds.getProperties().get(CabalSyntax.FIELD_EXTENSIONS);
    ls=PackageDescriptionLoader.parseList( val );
    assertEquals(2,ls.size());
    assertTrue(ls.contains( "UndecidableInstances" ));
    assertTrue(ls.contains( "OverlappingInstances" ));
  }

  public void testMultipleDocUpdate() throws IOException{
    String content3=getContent( "P1.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content3 );
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    assertEquals(4,pdss.size());
    PackageDescriptionStanza pds=pdss.get(2);
    TestDocument doc=new TestDocument( content3 );
    RealValuePosition rvp1=pds.update( CabalSyntax.FIELD_EXTENSIONS, "UndecidableInstances,"+PlatformUtil.NL+"OverlappingInstances" );
    rvp1.updateDocument( doc );
    RealValuePosition rvp2=pds.update( CabalSyntax.FIELD_BUILD_DEPENDS,"array,"+PlatformUtil.NL+"random");
    rvp2.updateDocument( doc );
    String s=doc.get();
    System.out.println(s);
    pd=PackageDescriptionLoader.load( s );
    pdss=pd.getStanzas();
    assertEquals(4,pdss.size());
    pds=pdss.get(2);
    String val=pds.getProperties().get(CabalSyntax.FIELD_BUILD_DEPENDS);
    List<String> ls=PackageDescriptionLoader.parseList( val );
    assertEquals(2,ls.size());
    assertTrue(ls.contains( "array" ));
    assertTrue(ls.contains( "random" ));
    val=pds.getProperties().get(CabalSyntax.FIELD_EXTENSIONS);
    ls=PackageDescriptionLoader.parseList( val );
    assertEquals(2,ls.size());
    assertTrue(ls.contains( "UndecidableInstances" ));
    assertTrue(ls.contains( "OverlappingInstances" ));
  }

  public void testParseDumpExample1KeepCase(){
    String content1=getContent( "Example1.cabal" );
    PackageDescription pd=PackageDescriptionLoader.load( content1 );
    String content2=pd.dump();
    assertEquals(content1.substring(0,5),content2.substring(0,5));
    assertTrue(content2,content2.contains("Library"));
  }

  public void testRemoveEntry(){
      String content1="name:     P1"+PlatformUtil.NL+
        "version:  0.1"+PlatformUtil.NL+
        "cabal-version: >= 1.2"+PlatformUtil.NL+
        "author:   JP2"+PlatformUtil.NL+
        "build-type: Simple"+PlatformUtil.NL+
        ""+PlatformUtil.NL+
        "executable P1"+PlatformUtil.NL+
        "  main-is:         Main.hs"+PlatformUtil.NL+
        "  ghc-options:     -O1 -fglasgow-exts -Wall"+PlatformUtil.NL+
        "  extensions:      OverlappingInstances"+PlatformUtil.NL+
        "  build-depends:   "+PlatformUtil.NL+
        "    multiset     >= 0.1 && < 0.3,"+PlatformUtil.NL+
        "    time, base"+PlatformUtil.NL+
        "  other-modules:   "+PlatformUtil.NL+
        "                   M2,"+PlatformUtil.NL+
        "                   M4"+PlatformUtil.NL;
      PackageDescription pd=PackageDescriptionLoader.load( content1 );
      List<PackageDescriptionStanza> pdss=pd.getStanzas();
      assertEquals(2,pdss.size());
      PackageDescriptionStanza pds=pdss.get(1);
      RealValuePosition rvp=pds.removeFromPropertyList( CabalSyntax.FIELD_EXTENSIONS, "OverlappingInstances" );
      TestDocument doc=new TestDocument( content1 );
      rvp.updateDocument( doc );
      String content2="name:     P1"+PlatformUtil.NL+
          "version:  0.1"+PlatformUtil.NL+
          "cabal-version: >= 1.2"+PlatformUtil.NL+
          "author:   JP2"+PlatformUtil.NL+
          "build-type: Simple"+PlatformUtil.NL+
          ""+PlatformUtil.NL+
          "executable P1"+PlatformUtil.NL+
          "  main-is:         Main.hs"+PlatformUtil.NL+
          "  ghc-options:     -O1 -fglasgow-exts -Wall"+PlatformUtil.NL+
          "  build-depends:   "+PlatformUtil.NL+
          "    multiset     >= 0.1 && < 0.3,"+PlatformUtil.NL+
          "    time, base"+PlatformUtil.NL+
          "  other-modules:   "+PlatformUtil.NL+
          "                   M2,"+PlatformUtil.NL+
          "                   M4"+PlatformUtil.NL;
      assertEquals(content2,doc.get());
      rvp=pds.removeFromPropertyList( CabalSyntax.FIELD_BUILD_DEPENDS, "time" );
      rvp.updateDocument( doc );
      String content3="name:     P1"+PlatformUtil.NL+
          "version:  0.1"+PlatformUtil.NL+
          "cabal-version: >= 1.2"+PlatformUtil.NL+
          "author:   JP2"+PlatformUtil.NL+
          "build-type: Simple"+PlatformUtil.NL+
          ""+PlatformUtil.NL+
          "executable P1"+PlatformUtil.NL+
          "  main-is:         Main.hs"+PlatformUtil.NL+
          "  ghc-options:     -O1 -fglasgow-exts -Wall"+PlatformUtil.NL+
          "  build-depends:   "+PlatformUtil.NL+
          "    multiset >= 0.1 && < 0.3,"+PlatformUtil.NL+
          "    base"+PlatformUtil.NL+
          "  other-modules:   "+PlatformUtil.NL+
          "                   M2,"+PlatformUtil.NL+
          "                   M4"+PlatformUtil.NL;
      assertEquals(content3,doc.get());
  }

  public void testRemoveEntryBySettingEmpty(){
    String content1="name:     P1"+PlatformUtil.NL+
      "version:  0.1"+PlatformUtil.NL+
      "cabal-version: >= 1.2"+PlatformUtil.NL+
      "author:   JP2"+PlatformUtil.NL+
      "build-type: Simple"+PlatformUtil.NL+
      ""+PlatformUtil.NL+
      "executable P1"+PlatformUtil.NL+
      "  main-is:         Main.hs"+PlatformUtil.NL+
      "  ghc-options:     -O1 -fglasgow-exts -Wall"+PlatformUtil.NL+
      "  extensions:      OverlappingInstances"+PlatformUtil.NL+
      "  build-depends:   "+PlatformUtil.NL+
      "    multiset     >= 0.1 && < 0.3,"+PlatformUtil.NL+
      "    time, base"+PlatformUtil.NL+
      "  other-modules:   "+PlatformUtil.NL+
      "                   M2,"+PlatformUtil.NL+
      "                   M4"+PlatformUtil.NL;
    PackageDescription pd=PackageDescriptionLoader.load( content1 );
    List<PackageDescriptionStanza> pdss=pd.getStanzas();
    assertEquals(2,pdss.size());
    PackageDescriptionStanza pds=pdss.get(1);
    RealValuePosition rvp=pds.update( CabalSyntax.FIELD_EXTENSIONS, "" );
    TestDocument doc=new TestDocument( content1 );
    rvp.updateDocument( doc );
    String content2="name:     P1"+PlatformUtil.NL+
        "version:  0.1"+PlatformUtil.NL+
        "cabal-version: >= 1.2"+PlatformUtil.NL+
        "author:   JP2"+PlatformUtil.NL+
        "build-type: Simple"+PlatformUtil.NL+
        ""+PlatformUtil.NL+
        "executable P1"+PlatformUtil.NL+
        "  main-is:         Main.hs"+PlatformUtil.NL+
        "  ghc-options:     -O1 -fglasgow-exts -Wall"+PlatformUtil.NL+
        "  build-depends:   "+PlatformUtil.NL+
        "    multiset     >= 0.1 && < 0.3,"+PlatformUtil.NL+
        "    time, base"+PlatformUtil.NL+
        "  other-modules:   "+PlatformUtil.NL+
        "                   M2,"+PlatformUtil.NL+
        "                   M4"+PlatformUtil.NL;
    assertEquals(content2,doc.get());
}
}
