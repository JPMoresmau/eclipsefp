/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.ReferenceLocation;
import net.sf.eclipsefp.haskell.buildwrapper.types.ThingAtPoint;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.texteditor.ITextEditor;


/**
 * @author JP Moresmau
 *
 */
public class ImportCleaner {

  public void cleanFile(final ITextEditor editor){
    if (editor instanceof HaskellEditor) {
      HaskellEditor hEditor = (HaskellEditor) editor;
      IFile f=hEditor.findFile();
      if (f!=null){
        BWFacade bwf=BuildWrapperPlugin.getFacade( f.getProject() );

        IDocument d= ( ( HaskellEditor )editor ).getDocument();
        Map<String,List<ReferenceLocation>> refs=BuildWrapperPlugin.getDefault().getUsageAPI().listReferencesInFile( f );
        HaskellUIPlugin.log( refs.toString(), IStatus.INFO );
        Map<String,String> translations=new HashMap<String, String>();
        Map<String,ReferenceLocation> allImports=getImportLines(bwf,f,refs,translations);
        HaskellUIPlugin.log( allImports.toString(), IStatus.INFO );

        Set<String> foundModules=new HashSet<String>();
        Map<String,Set<String>> foundSymbolsByModules=new HashMap<String, Set<String>>();
        Map<String,Set<String>> toAdd=new HashMap<String, Set<String>>();

        for (String section:refs.keySet()){
          if (!"import".equals( section )){
            for (ReferenceLocation loc:refs.get( section )){
              String ref=loc.getReference();
//
//              if (loc.isModule()){
//                String tr=GHC_PACKAGES_TO_BASE.get( ref );
//                if (tr!=null){
//                  ref=tr;
//                }
//              }
              String module=ref;
              ReferenceLocation moduleLoc=allImports.get( ref );
              if (!loc.isModule()){
                String tr=translations.get(ref);
                if(tr!=null){
                  ref=tr;
                  moduleLoc=allImports.get( ref );
                }
                int ix=ref.lastIndexOf( '.' );
                if (ix>-1){
                  module=ref.substring( 0,ix );
                  ref=ref.substring( ix+1 );
                  if (moduleLoc==null){
                    Set<String> s=toAdd.get( module );
                    if (s==null){
                      s=new HashSet<String>();
                      toAdd.put( module, s );
                    }
                    s.add( ref );
                    moduleLoc=allImports.get( module);
                  }
                  Set<String> s=foundSymbolsByModules.get( module );
                  if (s==null){
                    s=new HashSet<String>();
                    foundSymbolsByModules.put( module, s );
                  }
                  s.add( ref );
                }
              }

              if (moduleLoc!=null){
                foundModules.add( module );
              }
            }
          }
        }

        for (String symbol:allImports.keySet()){
          ReferenceLocation l=allImports.get( symbol );
          if (l.isModule()){
            try {
              if (l.getContents( d ).equals( symbol )){ // avoid removing/adding implicit prelude
                if (!foundModules.contains( symbol )){
                      HaskellUIPlugin.log( "toRemove module:"+symbol+":"+l, IStatus.INFO );
                } else {
                  Set<String> s=toAdd.get( symbol );
                  if (s!=null){
                    for (String ref:s){
                      HaskellUIPlugin.log( "toAdd symbol:"+symbol+"."+ref+":"+l, IStatus.INFO );
                    }
                  }
                }
              }
            } catch (BadLocationException ble){
              HaskellUIPlugin.log( ble );
            }
          } else {
            int ix=symbol.lastIndexOf( '.' );
            if (ix>-1){
              String module=symbol.substring( 0,ix );

              if (foundModules.contains( module )){
                String ref=symbol.substring( ix+1 );
                Set<String> s=foundSymbolsByModules.get( module );
                if (s==null || !s.contains( ref )){
                  HaskellUIPlugin.log( "toRemove symbol:"+symbol+":"+l, IStatus.INFO );
                }
              }
            }
          }
        }

      }
    }
  }

  private Map<String,ReferenceLocation> getImportLines(final BWFacade bwf,final IFile f,final Map<String,List<ReferenceLocation>> m,final Map<String,String> translations){
    Map<String,ReferenceLocation> ret=new HashMap<String, ReferenceLocation>();
    List<ReferenceLocation> imps=m.get( "import" );
    if (imps!=null){
      for (ReferenceLocation loc:imps){
         String ref=loc.getReference();
         if (bwf!=null && !loc.isModule()){
           ThingAtPoint tap=bwf.getThingAtPoint( f, loc );
           if (tap!=null){
             String nref=tap.getModule()+"."+tap.getName();
             translations.put( nref, ref );
             //ref=nref;
           }
         }
         ret.put( ref, loc );
      }
    }

    return ret;
  }

//  public static Map<String,String> GHC_PACKAGES_TO_BASE=new HashMap<String, String>();
//
//  static {
//    GHC_PACKAGES_TO_BASE.put( "GHC.Unicode", "Data.Char" );
//    GHC_PACKAGES_TO_BASE.put( "GHC.List", "Prelude" );
//    GHC_PACKAGES_TO_BASE.put( "GHC.Base", "Prelude" );
//    GHC_PACKAGES_TO_BASE.put( "GHC.Num", "Prelude" );
//  }

}
