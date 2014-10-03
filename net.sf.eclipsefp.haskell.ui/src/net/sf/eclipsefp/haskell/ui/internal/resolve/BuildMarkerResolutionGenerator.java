/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.resolve;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.Database;
import net.sf.eclipsefp.haskell.browser.items.DeclarationId;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.CabalMessages;
import net.sf.eclipsefp.haskell.buildwrapper.types.GhcMessages;
import net.sf.eclipsefp.haskell.buildwrapper.types.SearchResultLocation;
import net.sf.eclipsefp.haskell.buildwrapper.types.UsageResults;
import net.sf.eclipsefp.haskell.buildwrapper.usage.UsageQueryFlags;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.hlint.HLintFixer;
import net.sf.eclipsefp.haskell.hlint.Suggestion;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.backend.BackendManager;
import net.sf.eclipsefp.haskell.util.HaskellText;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.IMarkerResolutionGenerator;

/**
 * <p>Provides resolutions for markers</p>
  *
  * @author JP Moresmau
 */
public class BuildMarkerResolutionGenerator implements
    IMarkerResolutionGenerator {

  @Override
  public IMarkerResolution[] getResolutions( final IMarker marker ) {
    if (marker==null || !marker.exists()){
      return new IMarkerResolution[0];
    }
    List<IMarkerResolution> res=new ArrayList<>();

    IMarkerResolution hlr=getHLintResolution( marker );
    if (hlr!=null){
      res.add( hlr );
    } else {

      if (marker.getAttribute( IMarker.SEVERITY , IMarker.SEVERITY_ERROR)==IMarker.SEVERITY_WARNING || (marker.getAttribute( IMarker.SEVERITY , IMarker.SEVERITY_ERROR)==IMarker.SEVERITY_ERROR)){
        String msg=marker.getAttribute(IMarker.MESSAGE,""); //$NON-NLS-1$
        if (msg!=null){
          String msgL=msg.toLowerCase(Locale.ENGLISH);

          int ix=-1;

          // Type signature not found
          if (msgL.indexOf( GhcMessages.WARNING_NOTYPE_CONTAINS )>-1){
            res.add(new MissingTypeWarningResolution(GhcMessages.WARNING_INFERREDTYPE_START));
          } else if (msgL.indexOf( GhcMessages.WARNING_NOTYPE_TOPLEVEL_CONTAINS )>-1){
            // type is given on next line
            res.add(new MissingTypeWarningResolution(GhcMessages.WARNING_NOTYPE_TOPLEVEL_CONTAINS));
          }
          // Useless import
          else if (msgL.indexOf( GhcMessages.WARNING_IMPORT_USELESS_CONTAINS )>-1){
            res.add(new RemoveImportResolution());
            int ix2=msgL.indexOf( GhcMessages.WARNING_IMPORT_USELESS_START );
            if (ix2>-1){
              String newImport=msg.substring( ix2+GhcMessages.WARNING_IMPORT_USELESS_START.length() ).trim();
              res.add( new ReplaceImportResolution( newImport ) );
            }
          } else if (msgL.indexOf( GhcMessages.WARNING_IMPORT_USELESS_CONTAINS2 )>-1){
            int ixe2=-1;
            if ((ixe2=msgL.indexOf( GhcMessages.WARNING_IMPORT_USELESS_ELEMENT2 )) > -1) {
              // Redundant element
              // 1. Find redundant element
              int start=msgL.indexOf( GhcMessages.WARNING_IMPORT_USELESS_CONTAINS_START );
              if (start>-1){
                String redundantElement=msg.substring( start+GhcMessages.WARNING_IMPORT_USELESS_CONTAINS_START.length(), ixe2 ).trim();
                if (redundantElement.startsWith( "`" ) || redundantElement.startsWith( "‘" )){
                  redundantElement=redundantElement.substring( 1,redundantElement.length()-1);
                }
                //int backQuote1 = msg.indexOf( '`' );
                //int endQuote1 = msg.indexOf( '\'',backQuote1 );
                //String redundantElement = msg.substring( backQuote1 + 1, endQuote1 );
                /*String rest = msg.substring( endQuote1 + 1 );
                int backQuote2 = rest.indexOf( '`' );
                int endQuote2 = rest.indexOf( '\'' );
                String inImport = rest.substring( backQuote2 + 1, endQuote2 );*/
                res.add( new RemoveRedundantElementInImportResolution( redundantElement ) );
              }
            } else {
              // Redundant entire import
              res.add(new RemoveImportResolution());
              int ix2=msgL.indexOf( GhcMessages.WARNING_IMPORT_USELESS_START2 );
              if (ix2>-1){
                String newImport=msg.substring( ix2+GhcMessages.WARNING_IMPORT_USELESS_START2.length() ).trim();
                res.add( new ReplaceImportResolution( newImport ) );
              }
            }
          }
          // Language pragma needed
          else if (addFlagPragma(res,msg,msgL, GhcMessages.WARNING_USEFLAG_CONTAINS,GhcMessages.WARNING_USEFLAG_CONTAINS2,GhcMessages.WARNING_USEFLAG_CONTAINS3)){
            //
          } else if ((ix=msgL.indexOf( GhcMessages.WARNING_SUPPRESS_CONTAINS ))>-1){
             int end=ix-2;
             int ix2=msg.lastIndexOf( ' ',end);
             if (ix2>-1){
               String flag=msg.substring( ix2+1,end+1 ).trim();
               addPragma(res,flag);
             }
          } else if ((ix=msgL.indexOf( GhcMessages.NOT_ENABLED ))>1){
            String flag=msg.substring( 0,ix ).trim();
            res.add( new AddLanguagePragmaResolution( flag ) );
          } else if ((ix=msgL.indexOf( GhcMessages.PERMITS_THIS ))>1){
            int ix2=msg.substring(0,ix).lastIndexOf("(-X");
            String flag=msg.substring( ix2+1,ix ).trim();
            addPragma(res,flag);
          } else if ((ix=msgL.indexOf( GhcMessages.TRY ))>1){
            int ix2=msg.indexOf(" ",ix+GhcMessages.TRY.length());
            if (ix2>-1){
              String flag=msg.substring( ix+GhcMessages.TRY.length()-2,ix2).trim();
              addPragma(res,flag);
            }
          } else if ((ix=msgL.indexOf( GhcMessages.YOU_NEED ))>1){
            int ix2=msg.indexOf(" ",ix+GhcMessages.YOU_NEED.length());
            if (ix2>-1){
              String flag=msg.substring( ix+GhcMessages.YOU_NEED.length()-2,ix2).trim();
              addPragma(res,flag);
            }
          } else if ((ix=msgL.indexOf( GhcMessages.DID_YOU_MEAN ))>1){
            int ix2=msg.indexOf("?",ix+GhcMessages.DID_YOU_MEAN.length());
            if (ix2>-1){
              String flag=msg.substring( ix+GhcMessages.DID_YOU_MEAN.length()-2,ix2).trim();
              addPragma(res,flag);
            }
          } else if ((ix=msgL.indexOf( GhcMessages.DID_YOU_MEAN_NO_X ))>1){
            int ix2=msg.indexOf("?",ix+GhcMessages.DID_YOU_MEAN_NO_X.length());
            if (ix2>-1){
              String flag=msg.substring( ix+GhcMessages.DID_YOU_MEAN_NO_X.length(),ix2).trim();
              addPragma(res,flag);
            }
          } else if ((ix=msgL.indexOf( GhcMessages.CAST_FROM_CHAR ))>1){
            addPragma( res, "-XOverloadedStrings" );
          } else if ((ix=msgL.indexOf( GhcMessages.CAST_FROM_CHAR_SHORT ))>1){
            addPragma( res, "-XOverloadedStrings" );
          } else if ((ix=msgL.indexOf( GhcMessages.CAST_FROM_CHAR_7_8 ))>1){
            addPragma( res, "-XOverloadedStrings" );
          } else if ((ix=msgL.indexOf( GhcMessages.CAST_FROM_CHAR_SHORT_7_8 ))>1){
            addPragma( res, "-XOverloadedStrings" );
          }
          // Import a package
          else if (msgL.indexOf(GhcMessages.MISSING_MODULE)>-1){
            int start=GhcMessages.MISSING_MODULE.length();
            int length=GhcMessages.MISSING_MODULE_ADD_START.length();
            ix=msgL.indexOf( GhcMessages.MISSING_MODULE_ADD_START,start );
            if (ix==-1){
              ix=msgL.indexOf( GhcMessages.MISSING_MODULE_ADD_START_7_8,ix);
              length=GhcMessages.MISSING_MODULE_ADD_START_7_8.length();
            }
            if (ix>-1){
              Set<String> pkgs=new HashSet<>();
              while (ix>-1){
                int ix2=msgL.indexOf( GhcMessages.MISSING_MODULE_ADD_END,ix);
                if (ix2==-1){
                  ix2=msgL.indexOf( GhcMessages.MISSING_MODULE_ADD_END_7_8,ix);
                }
                if (ix2>-1){
                  String pkg=msg.substring( ix+length,ix2 );
                  // if the dependency can be found in several versions, we'll get several messages
                  if (pkgs.add( pkg )){
                    res.add(new AddPackageDependency( pkg ));
                  }
                  ix=ix2;
                }
                int st=ix+1;
                length=GhcMessages.MISSING_MODULE_ADD_START.length();
                ix=msgL.indexOf( GhcMessages.MISSING_MODULE_ADD_START,st );
                if (ix==-1){
                  ix=msgL.indexOf( GhcMessages.MISSING_MODULE_ADD_START_7_8,st);
                  length=GhcMessages.MISSING_MODULE_ADD_START_7_8.length();
                }
              }

            }
            int l=msgL.indexOf( "\n",start+1 );
            if (l>-1){
              String sug=msg.substring( l );
              List<String> suggestions=ReplaceTextResolution.getSuggestionsFromGHCMessage( sug,msgL.substring( l ) );
              msgL=msgL.substring( 0,l );
              int end = msgL.lastIndexOf( GhcMessages.NOT_IN_SCOPE_END );
              if (end==-1){
                end = msgL.lastIndexOf( GhcMessages.NOT_IN_SCOPE_END_7_8 );
              }
              String notInScope = msg.substring( start + 2, end );

              for (String suggestion:suggestions){
                res.add( new ReplaceTextResolution( notInScope, suggestion ) );
              }
            }
          }
          // Not in scope
          else if ((ix=msgL.indexOf( GhcMessages.NOT_IN_SCOPE_START) )>-1){
            ResolutionSuggestion s=new ResolutionSuggestion( msg, ix,msgL );
            if (s.getSuggestions()!=null){
              for (String suggestion:s.getSuggestions()){
                res.add( new ReplaceTextResolution( s.getOutOfScope(), suggestion ) );
              }
            }
            addBrowserSuggestions( marker, s.getOutOfScopeName(), s.getOutOfScopeQualifier(), res);
            addUsageSuggestions( marker, s.getOutOfScopeName() , s.getOutOfScopeQualifier(), res);
          } else if (msgL.indexOf( GhcMessages.IS_A_DATA_CONSTRUCTOR )>-1){
            int btix=msg.indexOf('`');
            int sqix=msg.indexOf('\'',btix);
            //String module=msg.substring(btix+1,sqix);
            btix=msg.indexOf('`',sqix);
            sqix=msg.indexOf('\'',btix);
            String constructor=msg.substring(btix+1,sqix);
            btix=msg.indexOf('`',sqix);
            sqix=msg.indexOf('\'',btix);
            String type=msg.substring(btix+1,sqix);
            res.add( new ReplaceImportElement( constructor, type+"("+constructor+")" ) );
            res.add( new ReplaceImportElement( constructor, type+"(..)" ) );
           /* btix=msg.indexOf('`',btix+1);
            sqix=msg.indexOf('\'',btix);
            String import1=msg.substring(btix+1,sqix);
            btix=msg.indexOf('`',sqix);
            sqix=msg.indexOf('\'',btix);
            String import2=msg.substring(btix+1,sqix);
            res.add(new ReplaceImportResolution( import1 ));
            res.add(new ReplaceImportResolution( import2 ));*/
            /*
            Description Resource  Path  Location  Type  ID
            In module `System.Exit':
              `ExitFailure' is a data constructor of `ExitCode'
            To import it use
              `import System.Exit (ExitCode (ExitFailure))'
            or
              `import System.Exit (ExitCode (..))'
              Main.hs /nxt/test line 16 Haskell Problem 39935*/
          }  else if (msgL.indexOf( GhcMessages.DO_DISCARDED_START )>-1){
            res.add(new AddGhcPragmaResolution( "-fno-warn-unused-do-bind" ));
            res.add(new AddGHCOptionResolution( "-fno-warn-unused-do-bind" ));
//            int fixIx=msgL.indexOf( GhcMessages.DO_DISCARDED_FIX );
//            if (fixIx>-1){
//
//            }
          } else if ((ix=msgL.indexOf( CabalMessages.DEPENDENCIES_MISSING ))>-1){
            // sandbox does the download for us, so if we're missing a dependency and sandbox,
            // either it's badly spelt or we don't have internet connnection...
            if (!BackendManager.getCabalImplDetails().isSandboxed()){
              int nlid=msg.indexOf( "\n",ix );
              Set<String> all=new HashSet<>();
              for (String s:msg.substring( nlid ).split( "\\n" )){
                s=s.trim();
                if (s.length()>0){
                  for (String p:s.split( "," )){
                    p=p.trim();
                    if (p.endsWith( CabalMessages.ANY)){
                      p=p.substring( 0,p.length()-CabalMessages.ANY.length() ).trim();
                    }
                    all.add(p);
                    res.add(new InstallMissingPackage( Collections.singleton( p ) ));
                  }
                }
              }
              if (all.size()>1){
                res.add(new InstallMissingPackage( all ));
              }
              // ... well it can happen that the configure has failed, so still give the option
            } else {
              res.add( new InstallDeps() );
            }
          } else if (msgL.indexOf( GhcMessages.NAKED )>-1){
            res.add( new AddLanguagePragmaResolution( "TemplateHaskell" ) );
          } else if (msgL.indexOf( GhcMessages.INPUT_CASE )>-1){
            res.add( new AddLanguagePragmaResolution( "LambdaCase" ) );
          } else if ((ix=msgL.indexOf( CabalMessages.CABAL_VERSION  ))>-1){
            ix+=CabalMessages.CABAL_VERSION.length();
            int ix2=msgL.indexOf( '\'', ix );
            if (ix2>-1){
              String value=msg.substring( ix,ix2 ).trim();
              if (value.length()>0){
                res.add(new CabalFieldSetter( CabalSyntax.FIELD_CABAL_VERSION, value ));
              }
            }
          } else if (msg.indexOf( "DeriveDataTypeable")>-1){
            res.add( new AddLanguagePragmaResolution( "DeriveDataTypeable" ) );
          }
        }
      }
    }

    return res.toArray( new IMarkerResolution[res.size()] );
  }

  /**
   * add suggestions from Browser
   * @param marker the current marked to fix
   * @param name the name
   * @param qualified the qualifier if any
   * @param res the suggestions
   */
  private void addBrowserSuggestions(final IMarker marker,String name,final String qualified,final List<IMarkerResolution> res){
    try {

      if (BrowserPlugin.getSharedInstance().isAnyDatabaseLoaded() && !BrowserPlugin.getSharedInstance().isRunning()) {
     // symbols are wrapped in (), so we want to make sure this is true
        if (name!=null && name.length()>0){
          char ch=name.charAt(0);
          if (!HaskellText.isHaskellIdentifierPart(ch) && ch!='('){
            name="("+name+")";
          }
        }
        DeclarationId[] availableMods = BrowserPlugin.getSharedInstance().findModulesForDeclaration(Database.ALL, name );

        /**
         * get the packages we know, we'll show declarations from these first
         */
        final Set<String> refs=getReferencedPackages(marker);

        Arrays.sort(availableMods,new Comparator<DeclarationId>() {
          /* (non-Javadoc)
           * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
           */
          @Override
          public int compare( final DeclarationId o1, final DeclarationId o2 ) {
            String m1=o1.getModule().getName();
            String m2=o2.getModule().getName();
            // known packages go first
            boolean isPkg1=refs.contains( o1.getPackageName() );
            boolean isPkg2=refs.contains( o2.getPackageName() );
            if (isPkg1){
              if (!isPkg2){
                return -1;
              }
            } else if (isPkg2){
              return 1;
            }

            int c=m1.compareToIgnoreCase( m2 );
            if (c==0){
              c=o1.getName().compareTo( o2.getName() );
            }
            return c;
          }
        });
        Set<String> modules=new HashSet<>();
        for (DeclarationId place : availableMods) {
          String module=place.getModule().getName();
          if (!modules.contains(module  )){
            modules.add(module);
            if (place.getName().length()>0){
              res.add( new AddImportResolution( place.getName()+"(..)", module, qualified ) );
              res.add( new AddImportResolution( place.getName()+"("+name+")", module, qualified ) );
            } else {
              res.add( new AddImportResolution( name, module, qualified ) );
            }
          }
        }
      }
    } catch (Exception e) {
      // Do nothing
    }
  }

  /**
   * add suggestions from the Usage db
   * @param marker the current marked to fix
   * @param name the name
   * @param qualified the qualifier if any
   * @param res the suggestions
   */
  private void addUsageSuggestions(final IMarker marker,final String name,final String qualified,final List<IMarkerResolution> res){
    if (name.length()>0 ){
      if (Character.isLowerCase( name.charAt( 0 ))){

        UsageResults ur=BuildWrapperPlugin.getDefault().getUsageAPI().likeSearch( null, name, null,UsageQueryFlags.TYPE_VAR, UsageQueryFlags.SCOPE_DEFINITIONS);
        for (IProject p:ur.listProjects()){
          for (IFile f:ur.getUsageInProject( p ).keySet()){
            String module=ResourceUtil.getModuleName( f );
            if (module!=null){
              res.add( new AddImportResolution( name, module, qualified ) );
            }
          }
        }
      } else {
        UsageResults ur=BuildWrapperPlugin.getDefault().getUsageAPI().likeSearch( null, name, null, UsageQueryFlags.TYPE_TYPE, UsageQueryFlags.SCOPE_DEFINITIONS);
        boolean found=false;
        for (IProject p:ur.listProjects()){
          for (IFile f:ur.getUsageInProject( p ).keySet()){
            String module=ResourceUtil.getModuleName( f );
            if (module!=null){
              res.add( new AddImportResolution( name, module, qualified ) );
              res.add( new AddImportResolution( name+"(..)", module, qualified ) );
              found=true;
            }
          }
        }
        if (!found){
          ur=BuildWrapperPlugin.getDefault().getUsageAPI().likeSearch( null, name, null, UsageQueryFlags.TYPE_CONSTRUCTOR, UsageQueryFlags.SCOPE_DEFINITIONS);
          for (IProject p:ur.listProjects()){
            Map<IFile,Map<String,Collection<SearchResultLocation>>> m=ur.getUsageInProject( p );
            for (IFile f:m.keySet()){
              String module=ResourceUtil.getModuleName( f );
              if (module!=null){
                for (String s:m.get(f).keySet()){
                  res.add( new AddImportResolution( s+"("+name+")", module, qualified ) );
                  res.add( new AddImportResolution( s+"(..)", module, qualified ) );
                  found=true;
                }
              }
            }
          }
        }
      }
    }
  }

  private IMarkerResolution getHLintResolution(final IMarker marker) {
    try {
      if (marker.getType().equals(HaskellCorePlugin.ID_HLINT_MARKER)){
        Suggestion s=new Suggestion();
        s.fromString( marker.getAttribute( HaskellCorePlugin.ATT_HLINT_SUGGESTION,"" ));
        if (HLintFixer.canFix( s )){
          return new HLintResolution();
        }
      }
    }catch (CoreException ce){
      HaskellUIPlugin.log( ce );
    }
    return null;
  }

  /**
   * add a pragma
   * @param res
   * @param msg
   * @param msgL
   * @param toSearch
   * @return true if a pragma was found
   */
  private boolean addFlagPragma(final List<IMarkerResolution> res,final String msg,final String msgL,final String... toSearch){
    int ix=-1;

    for (String s:toSearch){
      if ((ix=msgL.indexOf( s ))>-1){
        // start getting the pragma name from after the -x
        int ix2=ix+1+s.length();
        // we actually rely on -X to begin the pragma name later on
        if (s.endsWith( " -x" )){
          s=s.substring( s.length()-3 );
        } else {
          ix2+=3;
        }
        // we'll cut from start, but we'll look at the name from ix2...
        int start=ix+1+s.length();

        for (;ix2<msg.length();ix2++){
          // flags are letters and sometimes numbers (Rank2Types)
          if (!Character.isLetterOrDigit( msg.charAt( ix2 ) )){
            break;
          }
        }
        boolean ret=false;
         if (ix2<msg.length()){
          String flag=msg.substring( start,ix2 ).trim();
          ret=addPragma(res,flag);
        } else {
          String flag=msg.substring( start).trim();
          ret=addPragma(res,flag);
        }
         // return if we find something, otherwise look for another string
         if (ret){
           return true;
         }
      }
    }
    return false;
  }

  /**
   *
   * @param res
   * @param flag
   * @return true if the pragma was valid, false otherwise
   */
  private boolean addPragma(final List<IMarkerResolution> res,final String flag){
    if (flag!=null){
      if (flag.length()>2 && flag.startsWith( "-X" )){ //$NON-NLS-1$
        res.add( new AddLanguagePragmaResolution( flag.substring( 2 ) ) );
        return true;
      } else if (flag.length()>0 && Character.isUpperCase( flag.charAt( 0 ))){
        res.add( new AddLanguagePragmaResolution( flag ) );
        return true;
      }
    }
    return false;
  }

  private static Set<String> getReferencedPackages(final IMarker marker){
    IResource res=marker.getResource();
    Set<String> ret=new HashSet<>();
    if (res instanceof IFile){
      return ResourceUtil.getImportPackages( new IFile[]{(IFile)res} );
    }
    return ret;
  }
}
