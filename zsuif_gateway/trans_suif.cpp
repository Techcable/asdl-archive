#include <trans_suif.h>
#include "zsuif.hxx"
int TransSuif::do_it() {
    fprintf(stderr,"test\n");
    zsuif_suif_int* i = new zsuif_PlusInf();
    zsuif_suif_int::write(i,out);
    fclose(out);
    return 0;
  }

/*
       Copyright (c) 1998 Princeton University
*/
#include <suifnodes/suif.h>
#include "zsuif.hxx"







