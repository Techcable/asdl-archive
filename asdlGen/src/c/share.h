#ifndef _SHARE_
#define _SHARE_
#include "asdl_types.h"

#define SREF(x) x

share_ty ptr2share(void *ptr);
void *   share2ptr(share_ty s);
void share_clear_table(void);
#endif /* _SHARE */
