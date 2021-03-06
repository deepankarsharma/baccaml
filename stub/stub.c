#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>

extern void min_caml_sleep() asm ("min_caml_sleep");

extern int min_caml_get_micro_time() asm ("min_caml_get_micro_time");

extern void min_caml_start(char *, char *);

/* "stderr" is a macro and cannot be referred to in libmincaml.S, so */
/*    this "min_caml_stderr" is used (in place of "__iob+32") for better */
/*    portability (under SPARC emulators, for example).  Thanks to Steven */
/*    Shaw for reporting the problem and proposing this solution. */
FILE *min_caml_stderr;

// sleep in 1 second
void min_caml_sleep() {
  sleep(1);
  return;
}

// getting time in micro seconds
int min_caml_get_micro_time() {
  struct timeval current_time;
  gettimeofday(&current_time, NULL);
  return current_time.tv_sec * (int)1e6 + current_time.tv_usec;
}

int main() {
  char *hp, *sp;

  min_caml_stderr = stderr;
  sp = alloca(1000000); hp = malloc(4000000);
  if (hp == NULL || sp == NULL) {
    fprintf(stderr, "malloc or alloca failed\n");
    return 1;
  }
  /* fprintf(stderr, "sp = %p, hp = %p\n", sp, hp); */
  min_caml_start(sp, hp);

  return 0;
}
