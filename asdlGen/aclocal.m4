dnl check_smlnj.m4
dnl
dnl COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies.
dnl
dnl
dnl @synopsis AC_PROG_SMLNJ
dnl
dnl This macro figures out the location of SML/NJ and its major, minor,
dnl and patch version numbers.  The variables SMLNJ, SMLNJ_MAJOR_VERSION,
dnl SMLNJ_MINOR_VERSION, SMLNJ_PATCH_VERSION, SMLNJ_USING_NEW_CM, 
dnl and SMLNJ_CM_MAKE are set by this macro when it executes successfully.  
dnl This macro also does an AC_SUBST(SMLNJ) and AC_SUBST(SMLNJ_CM_MAKE). 
dnl You can override the version of SML/NJ used by defining the SMLNJ 
dnl variable in the  environment.
dnl 
dnl @version $Id$
dnl @author John Reppy <jhr@research.bell-labs.com>
dnl Modified by Daniel Wang
dnl
AC_DEFUN(AC_PROG_SMLNJ, [
dnl
dnl first we check for the existence of SML/NJ
dnl
  if test z$SMLNJ = z ; then
    AC_CHECK_PROGS(SMLNJ, sml-cm sml sml.bat, none)
  fi
  if test $SMLNJ = none; then
    AC_MSG_ERROR([sml not installed on PATH])
  else
dnl
dnl SML/NJ is installed, so determine the version numbers
dnl
   
   changequote(<<<,>>>)dnl
<<<$SMLNJ >/dev/null <<EOF
val _ = let
      val i2a = Int.toString
      val s = TextIO.openOut "conftest.sh"
      fun pr l = TextIO.output(s, concat l)
      in
        case #version_id Compiler.version
	 of [major] => (
	      pr["SMLNJ_MAJOR_VERSION=", i2a major, "\n"];
	      pr["SMLNJ_MINOR_VERSION=0\n"];
	      pr["SMLNJ_PATCH_VERSION=0\n"])
	  | [major, minor] => (
	      pr["SMLNJ_MAJOR_VERSION=", i2a major, "\n"];
	      pr["SMLNJ_MINOR_VERSION=", i2a minor, "\n"];
	      pr["SMLNJ_PATCH_VERSION=0\n"])
	  | (major::minor::patch::_) => (
	      pr["SMLNJ_MAJOR_VERSION=", i2a major, "\n"];
	      pr["SMLNJ_MINOR_VERSION=", i2a minor, "\n"];
	      pr["SMLNJ_PATCH_VERSION=", i2a patch, "\n"])
	  | _ => raise (Fail "unknown version")
	(* end case *);
      TextIO.closeOut s
      end;
EOF>>>
    changequote([, ])dnl
    . conftest.sh
    rm -fr "conftest.sh"
    AC_SUBST(SMLNJ)
  fi
dnl
dnl which version of CM are we using
dnl
  if test $SMLNJ_MAJOR_VERSION -eq 110 \
     -a $SMLNJ_MINOR_VERSION -eq 0 \
     -a $SMLNJ_PATCH_VERSION -ge 6
  then
    SMLNJ_USING_NEW_CM="FALSE"
  elif test $SMLNJ_MAJOR_VERSION -eq 110 \
    -a $SMLNJ_MINOR_VERSION -ge 32
  then
    SMLNJ_USING_NEW_CM="TRUE"
  else
    AC_MSG_ERROR([installation requires either version 110.0.6+ or 110.32+ of SML/NJ])
  fi
dnl
dnl if we are using the new version of CM, then we need to name
dnl the CM file explicitly.
dnl
  if test $SMLNJ_USING_NEW_CM = "TRUE" ; then
    SMLNJ_CM_MAKE="CM.make \"sources.cm\""
  else
    SMLNJ_CM_MAKE="CM.make ()"
  fi
  AC_SUBST(SMLNJ_CM_MAKE)
])dnl

