#! /bin/sh
# Guess the location of the sml runtime system 
prog=`which sml-cm`
case `basename $prog` in
 sml-cm.bat)
   #icck don't ask...
    eval "smldir=$(awk -F = '/@SET SMLNJ_HOME/ {print $2}' $prog|tr '\\' '/')"
    runtime=`echo ${smldir}/bin/.run/run.x86-win32.exe | cut -c3-`
 ;;
 *) runtime=`sh -x $prog 2>&1 < /dev/null | awk '/.*exec/ { print $3;}' ` 
 ;;
esac
name=`basename ${runtime}`
echo $runtime 
