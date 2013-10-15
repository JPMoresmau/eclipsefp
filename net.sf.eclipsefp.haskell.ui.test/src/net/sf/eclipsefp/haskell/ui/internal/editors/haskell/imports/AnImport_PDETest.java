/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import java.util.ArrayList;
import java.util.Arrays;
import net.sf.eclipsefp.haskell.buildwrapper.types.ImportDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.ImportExportType;
import net.sf.eclipsefp.haskell.buildwrapper.types.ImportSpecDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.core.cabalmodel.TestDocument;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.junit.Test;


/**
 * @author JP Moresmau
 *
 */
public class AnImport_PDETest {

  @Test
  public void testRemoveSimple(){
    // test in middle
    TestDocument td=new TestDocument( "module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence (Seq, singleton, empty)"+PlatformUtil.NL );
    Location loc=new Location( "TestSimpleRemove.hs",2,0,2,44);
    ImportDef def=new ImportDef( "Data.Sequence", loc, false, false, null );
    AnImport ai=new AnImport( def, false );
    CompletionProposal cp=ai.removeItem( td, "singleton" , "remove" );
    assertNotNull( cp );
    cp.apply( td );
    // test at end
    assertEquals("module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence (Seq, empty)"+PlatformUtil.NL,td.get());
    td=new TestDocument( "module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence (Seq, singleton, empty)"+PlatformUtil.NL );
    cp=ai.removeItem( td, "empty" , "remove" );
    assertNotNull( cp );
    cp.apply( td );
    // test at start + a name that appears in the module
    assertEquals("module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence (Seq, singleton)"+PlatformUtil.NL,td.get());
    td=new TestDocument( "module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence (Seq, singleton, empty)"+PlatformUtil.NL );
    cp=ai.removeItem( td, "Seq" , "remove" );
    assertNotNull( cp );
    cp.apply( td );
    assertEquals("module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence (singleton, empty)"+PlatformUtil.NL,td.get());
  }

  @Test
  public void testRemoveConstructor(){
    TestDocument td=new TestDocument( "module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence (Seq(..), singleton, empty)"+PlatformUtil.NL );
    Location loc=new Location( "TestSimpleRemove.hs",2,0,2,48);
    ImportDef def=new ImportDef( "Data.Sequence", loc, false, false, null );
    AnImport ai=new AnImport( def, false );
    CompletionProposal cp=ai.removeItem( td, "Seq" , "remove" );
    assertNotNull( cp );
    cp.apply( td );
    assertEquals("module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence (singleton, empty)"+PlatformUtil.NL,td.get());

  }

  @Test
  public void testRemoveConstructorAtEnd(){
    TestDocument td=new TestDocument( "module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence (singleton, empty, Seq(..))"+PlatformUtil.NL );
    Location loc=new Location( "TestSimpleRemove.hs",2,0,2,48);
    ImportDef def=new ImportDef( "Data.Sequence", loc, false, false, null );
    AnImport ai=new AnImport( def, false );
    CompletionProposal cp=ai.removeItem( td, "Seq" , "remove" );
    assertNotNull( cp );
    cp.apply( td );
    assertEquals("module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence (singleton, empty)"+PlatformUtil.NL,td.get());

  }

  @Test
  public void testRemoveMultipleAtEnd(){
    TestDocument td=new TestDocument( "module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence (singleton, empty, Seq(..))"+PlatformUtil.NL );
    Location loc=new Location( "TestSimpleRemove.hs",2,0,2,48);
    ImportDef def=new ImportDef( "Data.Sequence", loc, false, false, null );
    AnImport ai=new AnImport( def, false );
    CompletionProposal cp=ai.removeItem( td, Arrays.asList("Seq","singleton") , "remove" );
    assertNotNull( cp );
    cp.apply( td );
    assertEquals("module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence (empty)"+PlatformUtil.NL,td.get());

  }

  @Test
  public void testRemoveLast(){
    TestDocument td=new TestDocument( "module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence (Seq)"+PlatformUtil.NL );
    Location loc=new Location( "TestSimpleRemove.hs",2,0,2,26);
    ImportDef def=new ImportDef( "Data.Sequence", loc, false, false, null );
    AnImport ai=new AnImport( def, false );
    CompletionProposal cp=ai.removeItem( td, "Seq" , "remove" );
    assertNotNull( cp );
    cp.apply( td );
    assertEquals("module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence ()"+PlatformUtil.NL,td.get());
    td=new TestDocument( "module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence (Seq )"+PlatformUtil.NL );
    loc=new Location( "TestSimpleRemove.hs",2,0,2,27);
    def=new ImportDef( "Data.Sequence", loc, false, false, null );
    ai=new AnImport( def, false );
    cp=ai.removeItem( td, "Seq" , "remove" );
    assertNotNull( cp );
    cp.apply( td );
    assertEquals("module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence ()"+PlatformUtil.NL,td.get());
    td=new TestDocument( "module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence ( Seq)"+PlatformUtil.NL );
    cp=ai.removeItem( td, "Seq" , "remove" );
    assertNotNull( cp );
    cp.apply( td );
    assertEquals("module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence ()"+PlatformUtil.NL,td.get());
  }

  @Test
  public void testRemoveOperator(){
    TestDocument td=new TestDocument( "module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence ((><), Seq)"+PlatformUtil.NL );
    Location loc=new Location( "TestSimpleRemove.hs",2,0,2,32);
    ImportDef def=new ImportDef( "Data.Sequence", loc, false, false, null );
    AnImport ai=new AnImport( def, false );
    CompletionProposal cp=ai.removeItem( td, "><" , "remove" );
    assertNotNull( cp );
    cp.apply( td );
    assertEquals("module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence (Seq)"+PlatformUtil.NL,td.get());
  }

  @Test
  public void testRemoveSuperStringBefore(){
    TestDocument td=new TestDocument( "module TestSimpleRemove where"+PlatformUtil.NL+"import Control.Monad.Trans.Reader (runReaderT,runReader)"+PlatformUtil.NL );
    Location loc=new Location( "testRemoveSuperString.hs",2,0,2,56);
    ImportDef def=new ImportDef( "Control.Monad.Trans.Reader", loc, false, false, null );
    AnImport ai=new AnImport( def, false );
    CompletionProposal cp=ai.removeItem( td, "runReader" , "remove" );
    assertNotNull( cp );
    cp.apply( td );
    assertEquals("module TestSimpleRemove where"+PlatformUtil.NL+"import Control.Monad.Trans.Reader (runReaderT)"+PlatformUtil.NL,td.get());
  }

  @Test
  public void testRemoveSuperStringAfter(){
    TestDocument td=new TestDocument( "module TestSimpleRemove where"+PlatformUtil.NL+"import Control.Monad.Trans.Reader (runReader,runReaderT)"+PlatformUtil.NL );
    Location loc=new Location( "testRemoveSuperString.hs",2,0,2,56);
    ImportDef def=new ImportDef( "Control.Monad.Trans.Reader", loc, false, false, null );
    AnImport ai=new AnImport( def, false );
    CompletionProposal cp=ai.removeItem( td, "runReader" , "remove" );
    assertNotNull( cp );
    cp.apply( td );
    assertEquals("module TestSimpleRemove where"+PlatformUtil.NL+"import Control.Monad.Trans.Reader (runReaderT)"+PlatformUtil.NL,td.get());
  }

  @Test
  public void testAddSimple(){
    TestDocument td=new TestDocument( "module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence ((><), Seq)"+PlatformUtil.NL );
    Location loc=new Location( "TestSimpleRemove.hs",2,0,2,32);
    ImportDef def=new ImportDef( "Data.Sequence", loc, false, false, null );
    ImportSpecDef sp1=new ImportSpecDef( "><", new Location( "TestSimpleRemove.hs",2,22,2,26), ImportExportType.IEVar );
    ImportSpecDef sp2=new ImportSpecDef( "Seq", new Location( "TestSimpleRemove.hs",2,27,2,31), ImportExportType.IEVar );
    def.setChildren( new ArrayList<ImportSpecDef>() );
    def.getChildren().add(sp1);
    def.getChildren().add(sp2);
    AnImport ai=new AnImport( def, false );
    CompletionProposal cp=ai.addItem(td, "singleton" , "add" );
    assertNotNull( cp );
    cp.apply( td );
    assertEquals("module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence ((><), Seq, singleton)"+PlatformUtil.NL,td.get());
  }

  /**
   * this should never happen, if we import everything we're not missing one item, but ensure it's ok
   */
  @Test
  public void testAddToNoChildren(){
    TestDocument td=new TestDocument( "module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence"+PlatformUtil.NL );
    Location loc=new Location( "TestSimpleRemove.hs",2,0,2,20);
    ImportDef def=new ImportDef( "Data.Sequence", loc, false, false, null );
    AnImport ai=new AnImport( def, false );
    CompletionProposal cp=ai.addItem(td, "singleton" , "add" );
    assertNotNull( cp );
    cp.apply( td );
    assertEquals("module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence (singleton)"+PlatformUtil.NL,td.get());
  }

  @Test
  public void testAddToEmptyChildren(){
    TestDocument td=new TestDocument( "module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence ()"+PlatformUtil.NL );
    Location loc=new Location( "TestSimpleRemove.hs",2,0,2,23);
    ImportDef def=new ImportDef( "Data.Sequence", loc, false, false, null );
    def.setChildren( new ArrayList<ImportSpecDef>() );
    AnImport ai=new AnImport( def, false );
    CompletionProposal cp=ai.addItem(td, "singleton" , "add" );
    assertNotNull( cp );
    cp.apply( td );
    assertEquals("module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence (singleton)"+PlatformUtil.NL,td.get());
  }

  @Test
  public void testAddOperator(){
    TestDocument td=new TestDocument( "module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence (Seq)"+PlatformUtil.NL );
    Location loc=new Location( "TestSimpleRemove.hs",2,0,2,26);
    ImportDef def=new ImportDef( "Data.Sequence", loc, false, false, null );
    ImportSpecDef sp1=new ImportSpecDef( "Seq", new Location( "TestSimpleRemove.hs",2,24,2,25), ImportExportType.IEVar );
    def.setChildren( new ArrayList<ImportSpecDef>() );
    def.getChildren().add(sp1);
    AnImport ai=new AnImport( def, false );
    CompletionProposal cp=ai.addItem(td, "><" , "add" );
    assertNotNull( cp );
    cp.apply( td );
    assertEquals("module TestSimpleRemove where"+PlatformUtil.NL+"import Data.Sequence (Seq, (><))"+PlatformUtil.NL,td.get());
  }

}
