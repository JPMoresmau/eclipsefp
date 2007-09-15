#!/bin/bash
#
# Script for building EclipseFP headless
#
# Author: Thiago Arrais - thiago.arrais@gmail.com
# 
# Largelly inspired by the Ruby Development tools build script

ECLIPSE_HOME=${ECLIPSE_HOME:-/opt/eclipse/platform/3.1.2/eclipse}

echo using ECLIPSEFP_HOME: ${ECLIPSEFP_HOME:?must be set}
echo using ECLIPSE_HOME: $ECLIPSE_HOME
echo
pdeBuildPluginVersion=3.2.1.r321_v20060823
branchID="1.100.0"
buildDirectory=${ECLIPSEFP_BUILD_TARGET_DIR:-/tmp/eclipsefp-build}
vm=${ECLIPSEFP_JAVA_INTERPRETER:-java}

dateTag=`date +%Y%m%d-%H%M`

buildId=I${dateTag}

os=${ECLIPSEFP_OS:-linux}
ws=${ECLIPSEFP_WS:-gtk}
arch=${ECLIPSEFP_ARCH:-x86}

# Reset ant command line args
ANT_CMD_LINE_ARGS=

buildfile=$ECLIPSE_HOME/plugins/org.eclipse.pde.build_$pdeBuildPluginVersion/scripts/build.xml

# Make sure the buildDirectory isn't filled
rm -Rf $buildDirectory
mkdir $buildDirectory

if ! $vm -version > /dev/null 2>&1; then
    echo "Java VM not found. Aborting build..."
    exit
fi

if ! test -e $ECLIPSE_HOME/startup.jar; then
    echo "Eclipse installation not found. Aborting build..."
    exit
fi

if ! darcs --version > /dev/null 2>&1; then
    echo "Darcs not found. Aborting build..."
    exit
fi 

verifyCmd="$vm -cp $ECLIPSE_HOME/startup.jar \
org.eclipse.core.launcher.Main \
-application net.sf.eclipsefp.haskell.build.check.verifypreconditions \
-os linux -ws gtk -arch x86"

if ! $verifyCmd; then
    echo "Build preconditions not met. Aborting build..."
    echo "Are you sure you installed the net.sf.eclipsefp.haskell.build.check \
plugin to the Eclipse platform at $ECLIPSE_HOME?"
    exit
fi

echo Starting eclipse in $ECLIPSE_HOME, $vm
echo

cmd="$vm -cp $ECLIPSE_HOME/startup.jar \
org.eclipse.core.launcher.Main \
-application org.eclipse.ant.core.antRunner \
-buildfile $buildfile \
-Dbasews=$ws -Dbaseos=$os -Dbasearch=$arch \
-DjavacSource=1.5 \
-DjavacTarget=1.5 \
-DjavacFailOnError=true \
-Dpde.build.scripts=$ECLIPSE_HOME/plugins/org.eclipse.pde.build_$pdeBuildPluginVersion/scripts \
-DbaseLocation=$ECLIPSE_HOME \
-Dbuilder=$ECLIPSEFP_HOME/net.sf.eclipsefp.haskell.build \
-DbuildDirectory=$buildDirectory \
-DbuildId=$buildId \
-DdoGenerateUpdateSite=true \
-DskipFetch=true"
echo $cmd

remoteUser=${PUBLISH_USER:-tbasouza}
remoteServer=${PUBLISH_SERVER:-shell.sf.net}
remoteSiteRoot=${PUBLISH_SERVER_SITE_ROOT:-/home/groups/e/ec/eclipsefp/htdocs}

if $cmd > $buildDirectory/build.log; then
    mv $buildDirectory/build.log \
       $buildDirectory/I.$buildId/compilelogs/build.log
    tar czf $buildDirectory/net.sf.eclipsefp.haskell-${buildId}.zip.log.tar.gz \
            -C$buildDirectory/I.$buildId \
            compilelogs
    scp $buildDirectory/I.$buildId/net.sf.eclipsefp.haskell-${buildId}.zip \
        $buildDirectory/net.sf.eclipsefp.haskell-${buildId}.zip.log.tar.gz \
        $remoteUser@$remoteServer:$remoteSiteRoot/download/drops/$branchID/
fi
