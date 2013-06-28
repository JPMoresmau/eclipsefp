package net.sf.eclipsefp.haskell.ui.internal.resolve;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.Database;
import net.sf.eclipsefp.haskell.browser.items.DeclarationId;
import net.sf.eclipsefp.haskell.buildwrapper.types.CabalMessages;
import net.sf.eclipsefp.haskell.buildwrapper.types.GhcMessages;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.hlint.HLintFixer;
import net.sf.eclipsefp.haskell.hlint.Suggestion;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.scion.ScionManager;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
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
    List<IMarkerResolution> res=new ArrayList<IMarkerResolution>();

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
            if (msgL.indexOf( GhcMessages.WARNING_IMPORT_USELESS_ELEMENT2 ) > -1) {
              // Redundant element
              // 1. Find redundant element
              int backQuote1 = msg.indexOf( '`' );
              int endQuote1 = msg.indexOf( '\'' );
              String redundantElement = msg.substring( backQuote1 + 1, endQuote1 );
              /*String rest = msg.substring( endQuote1 + 1 );
              int backQuote2 = rest.indexOf( '`' );
              int endQuote2 = rest.indexOf( '\'' );
              String inImport = rest.substring( backQuote2 + 1, endQuote2 );*/
              res.add( new RemoveRedundantElementInImportResolution( redundantElement ) );
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
          } else if ((ix=msgL.indexOf( GhcMessages.CAST_FROM_CHAR ))>1){
            addPragma( res, "-XOverloadedStrings" );
          }
          // Import a package
          else if (msgL.indexOf(GhcMessages.MISSING_MODULE)>-1){
            int start=GhcMessages.MISSING_MODULE.length();
            ix=msgL.indexOf( GhcMessages.MISSING_MODULE_ADD_START,start );
            Set<String> pkgs=new HashSet<String>();
            while (ix>-1){
              int ix2=msgL.indexOf( GhcMessages.MISSING_MODULE_ADD_END,ix);
              if (ix2>-1){
                String pkg=msg.substring( ix+GhcMessages.MISSING_MODULE_ADD_START.length(),ix2 );
                // if the dependency can be found in several versions, we'll get several messages
                if (pkgs.add( pkg )){
                  res.add(new AddPackageDependency( pkg ));
                }
                ix=ix2;
              }
              ix=msgL.indexOf( GhcMessages.MISSING_MODULE_ADD_START,ix+1 );
            }
          }
          // Not in scope
          else if (msgL.indexOf( GhcMessages.NOT_IN_SCOPE_START )>-1){
            int start = msgL.indexOf( '`',msgL.indexOf( GhcMessages.NOT_IN_SCOPE_START ));
            int l=msgL.indexOf( "\n",start+1 );
            List<String> suggestions=null;
            if (l>-1){
              String sug=msg.substring( l );
              suggestions=ReplaceTextResolution.getSuggestionsFromGHCMessage( sug,msgL.substring( l ) );
              msgL=msgL.substring( 0,l );
            }
            int end = msgL.lastIndexOf( GhcMessages.NOT_IN_SCOPE_END );
            String notInScope = msg.substring( start + 1, end );
            String name, qualified;
            int pointPos = notInScope.lastIndexOf( '.' );
            if (pointPos != -1) {
              name = notInScope.substring( pointPos + 1 );
              qualified = notInScope.substring( 0, pointPos );
            } else {
              name = notInScope;
              qualified = null;
            }
            if (suggestions!=null){
              for (String suggestion:suggestions){
                res.add( new ReplaceTextResolution( notInScope, suggestion ) );
              }
            }
            try {

              if (BrowserPlugin.getSharedInstance().isAnyDatabaseLoaded() && !BrowserPlugin.getSharedInstance().isRunning()) {
                //BrowserPlugin.getSharedInstance().setCurrentDatabase( DatabaseType.ALL, null );
                // need to have the package
                DeclarationId[] availableMods = BrowserPlugin.getSharedInstance().findModulesForDeclaration(Database.ALL, name );
                /*ArrayList<String> places = new ArrayList<String>();
                for (DeclarationId avMod : availableMods) {
                  if (!places.contains( avMod.getName() )) {
                    places.add( avMod.getName() );
                  }
                }
                Collections.sort( places );*/
                Set<String> refs=getReferencedPackages(marker);

                Arrays.sort(availableMods,new Comparator<DeclarationId>() {
                  /* (non-Javadoc)
                   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
                   */
                  @Override
                  public int compare( final DeclarationId o1, final DeclarationId o2 ) {
                    String m1=o1.getModule().getName();
                    String m2=o2.getModule().getName();

                    int c=m1.compareToIgnoreCase( m2 );
                    if (c==0){
                      c=o1.getName().compareTo( o2.getName() );
                    }
                    return c;
                  }
                });
                Set<String> modules=new HashSet<String>();
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
            if (!ScionManager.getCabalImplDetails().isSandboxed()){
              int nlid=msg.indexOf( "\n",ix );
              Set<String> all=new HashSet<String>();
              for (String s:msg.substring( nlid ).split( "\\n" )){
                s=s.trim();
                if (s.length()>0){
                  if (s.endsWith( CabalMessages.ANY)){
                    s=s.substring( 0,s.length()-CabalMessages.ANY.length() ).trim();
                  }
                  all.add(s);
                  res.add(new InstallMissingPackage( Collections.singleton( s ) ));
                }
              }
              if (all.size()>1){
                res.add(new InstallMissingPackage( all ));
              }
            }
          }
        }
      }
    }

    return res.toArray( new IMarkerResolution[res.size()] );
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

  private boolean addFlagPragma(final List<IMarkerResolution> res,final String msg,final String msgL,final String... toSearch){
    int ix=-1;

    for (String s:toSearch){
      if ((ix=msgL.indexOf( s ))>-1){
        if (s.endsWith( " -x" )){
          s=s.substring( s.length()-3 );
        }
        int start=ix+1+s.length();
        int ix2=start;
        for (;ix2<msg.length();ix2++){
          if (Character.isWhitespace( msg.charAt( ix2 ) )){
            break;
          }
        }
         if (ix2<msg.length()){
          String flag=msg.substring( start,ix2 ).trim();
          addPragma(res,flag);
        } else {
          String flag=msg.substring( start).trim();
          addPragma(res,flag);
        }
        return true;
      }
    }
    return false;
  }

  private void addPragma(final List<IMarkerResolution> res,final String flag){
    if (flag!=null && flag.length()>2 && flag.startsWith( "-X" )){ //$NON-NLS-1$
      res.add( new AddLanguagePragmaResolution( flag.substring( 2 ) ) );
    }
  }

  private static Set<String> getReferencedPackages(final IMarker marker){
    IResource res=marker.getResource();
    Set<String> ret=new HashSet<String>();
    if (res instanceof IFile){
      return ResourceUtil.getImportPackages( new IFile[]{(IFile)res} );
    }
    return ret;
  }
}
