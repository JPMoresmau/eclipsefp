#!/bin/bash
#
# Script for building EclipseFP headless
#
# Author: Thiago Arrais - thiago.arrais@gmail.com
# 
# Largelly inspired by the Ruby Development tools build script

ECLIPSE_HOME=${ECLIPSE_HOME:-/opt/eclipse/platform/3.1.1/eclipse}

echo using ECLIPSEFP_HOME: ${ECLIPSEFP_HOME:?must be set}
echo using ECLIPSE_HOME: $ECLIPSE_HOME
pdeBuildPluginVersion=3.1.0
buildDirectory=${ECLIPSEFP_BUILD_TARGET_DIR:-${ECLIPSEFP_HOME}/tmp/build}
vm=${ECLIPSEFP_JAVA_INTERPRETER:-java}

os=${ECLIPSEFP_OS:-linux}
ws=${ECLIPSEFP_WS:-gtk}
arch=${ECLIPSEFP_ARCH:-x86}

# Reset ant command line args
ANT_CMD_LINE_ARGS=

buildfile=$ECLIPSE_HOME/plugins/org.eclipse.pde.build_$pdeBuildPluginVersion/scripts/build.xml

echo Starting eclipse in $eclipseDir, $vm
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
-DskipFetch=true"
echo $cmd
exec $cmd
