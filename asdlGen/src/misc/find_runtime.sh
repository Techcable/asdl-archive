#! /bin/sh
# Guess the location of the sml runtime system 
prog=`which sml`
runtime=`sh -x $prog 2>&1 < /dev/null | awk '/.*exec/ { print $3;}' `
echo $runtime
