#include <sys/statfs.h>
#include <stdio.h>

int main ()

{
  printf("%d\n", sizeof(struct statfs));
}
