#include <stdio.h>
static char * msg = "Hello World!\n";
static void hw(void) {
  printf(msg);
}
int main(int argc, char **argv){
  hw();
  return 0;
}
