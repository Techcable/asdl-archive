/* macros for matching ASDL code in C */
#ifndef _Match_
#define _Match_
#include "assert.h"
/* match an unboxed value */
#define MATCH_BEGINU(x) { switch(x) { 

  /* match a boxed value */
#define MATCH_BEGINB(x) { switch(x->kind) { 

  /* match unboxed constant */
#define MATCHU(x,con) case con##_enum:

  /* readonly boxed match */
#define MATCHR(x,con,y) case con##_enum: struct con##_s y = (x->v.con);
  /*  write boxed matchmatch */
#define MATCHW(x,con,y) case con##_enum: struct con##_s *y = &(x->v.con);
#define MATCH_END default: assert(0); }

/* defaults */
#define MATCH_BEGIN MATCHB
#define MATCH MATCHR
#endif
