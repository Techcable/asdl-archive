#include <stdio.h>
static char * msg = "Hello World!\n";
static void hw(void) {
  printf(msg);
}
int main(int argc, char **argv){
  int x;
  hw();
  x = 1;
  x = 2;
  x = 3;
  return 0;
}
