/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */


/*
 * SUIF to C converter
 */
#include "trans_suif.h"

FILE *pkl_file = stdout;

static bool debug_it = false;
static void parse_arguments(int argc, char *argv[], char **input_filespec, char **output_filespec);
static void usage(void);

FILE*
open_pkl_file(char *name)
{
    FILE *f = fopen(name, "wb");
    if (f == NULL) {
        fprintf(stderr, "Could not open %s for writing.\n", name);
        exit(1);
    }
    return f;
}


extern int
main(int argc, char **argv)
{

    char* input_filespec;
    char* output_filespec = NULL;
    SuifEnv* suif = new SuifEnv();
    suif->init();
    init_basicnodes(suif);
    init_suifnodes(suif);
    init_cfenodes(suif);

    parse_arguments(argc, argv, &input_filespec, &output_filespec);

    pkl_file = open_pkl_file(output_filespec);
    suif->read(input_filespec);

    //FormattedText x;
    //suif->get_file_set_block()->print(x);
    //cout << x.get_value() << endl;

    FileSetBlock *fsb = suif->get_file_set_block();
    TransSuif ts(pkl_file,suif,fsb);
    ts.trans_suif();
    exit(0);
}


static void parse_arguments(int argc, char *argv[], 
char **input_filespec, char **output_filespec)
{
    while (argc > 1)
    {
        const char *x = argv[1];
        if (strcmp(x,"-d") == 0)
            debug_it = true;
        else
            break;
        argc --;
        argv ++;
    }

    if (argc < 2)
    {
        fprintf(stderr, "No input SUIF file specified.\n");
        usage();
        exit(1);
    }

    *input_filespec = argv[1];
    *output_filespec = argv[2];
}


static void usage(void)
{
    char *usage_message[] =
    {
        "usage: s2c <options> <SUIF-file> <ZSUIF-file>\n",
        "  options:\n",
        "    -d debug macro expansion\n"
    };

    for (size_t line_num = 0;
         line_num < sizeof(usage_message) / sizeof(char *); ++line_num)
      {
        fprintf(stderr, "%s", usage_message[line_num]);
      }
}
