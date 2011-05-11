package net.sf.eclipsefp.haskell.ui.internal.resolve;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.scion.types.GhcMessages;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.IMarkerResolutionGenerator;

/**
 * <p>Provides resolutions for markers</p>
  *
  * @author JP Moresmau
 */
public class BuildMarkerResolutionGenerator implements
    IMarkerResolutionGenerator {


  public IMarkerResolution[] getResolutions( final IMarker marker ) {
    List<IMarkerResolution> res=new ArrayList<IMarkerResolution>();
    if (marker.getAttribute( IMarker.SEVERITY , IMarker.SEVERITY_ERROR)==IMarker.SEVERITY_WARNING || (marker.getAttribute( IMarker.SEVERITY , IMarker.SEVERITY_ERROR)==IMarker.SEVERITY_ERROR)){
      String msg=marker.getAttribute(IMarker.MESSAGE,""); //$NON-NLS-1$
      if (msg!=null){
        String msgL=msg.toLowerCase();
        if (marker.getResource() instanceof IFile){

          int ix=-1;
          if (msgL.indexOf( GhcMessages.WARNING_NOTYPE_CONTAINS )>-1){
            res.add(new MissingTypeWarningResolution(GhcMessages.WARNING_INFERREDTYPE_START));
          } else if (msgL.indexOf( GhcMessages.WARNING_NOTYPE_TOPLEVEL_CONTAINS )>-1){
            // type is given on next line
            res.add(new MissingTypeWarningResolution("\n"));
          } else if (msgL.indexOf( GhcMessages.WARNING_IMPORT_USELESS_CONTAINS )>-1){
            res.add(new RemoveImportResolution());
            int ix2=msgL.indexOf( GhcMessages.WARNING_IMPORT_USELESS_START );
            if (ix2>-1){
              String newImport=msg.substring( ix2+GhcMessages.WARNING_IMPORT_USELESS_START.length() ).trim();
              res.add( new ReplaceImportResolution( newImport ) );
            }
          } else if (addFlagPragma(res,msg,msgL, GhcMessages.WARNING_USEFLAG_CONTAINS,GhcMessages.WARNING_USEFLAG_CONTAINS2,GhcMessages.WARNING_USEFLAG_CONTAINS3)){
            //
          } else if ((ix=msgL.indexOf( GhcMessages.WARNING_SUPPRESS_CONTAINS ))>-1){
             int end=ix-2;
             int ix2=msg.lastIndexOf( ' ',end);
             if (ix2>-1){
               String flag=msg.substring( ix2+1,end+1 ).trim();
               addPragma(res,flag);
             }
          }
        } else if (marker.getResource() instanceof IProject){
          if (msgL.indexOf(GhcMessages.MISSING_MODULE)>-1){
            int start=GhcMessages.MISSING_MODULE.length();
            int ix=msgL.indexOf( GhcMessages.MISSING_MODULE_ADD_START,start );
            while (ix>-1){
              int ix2=msgL.indexOf( GhcMessages.MISSING_MODULE_ADD_END,ix);
              if (ix2>-1){
                String pkg=msgL.substring( ix+GhcMessages.MISSING_MODULE_ADD_START.length(),ix2 );
                res.add(new AddPackageDependency( pkg ));
                ix=ix2;
              }
              ix=msgL.indexOf( GhcMessages.MISSING_MODULE_ADD_START,ix+1 );
            }
          }
        }
      }
    }

    return res.toArray( new IMarkerResolution[res.size()] );
  }

  private boolean addFlagPragma(final List<IMarkerResolution> res,final String msg,final String msgL,final String... toSearch){
    int ix=-1;

    for (String s:toSearch){
      if ((ix=msgL.indexOf( s ))>-1){
        if (s.endsWith( " -x" )){
          s=s.substring( s.length()-3 );
        }
        int start=ix+1+s.length();
        int ix2=msg.indexOf( ' ',start);
        if (ix2>-1){
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
      res.add( new AddLanguagePramaResolution( flag.substring( 2 ) ) );
    }
  }


}
