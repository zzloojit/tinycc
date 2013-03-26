#include <pcre.h>
#include <string.h>
#include <stdio.h>

#define OVECCOUNT 30

int main()
{
  const char* error;
  int erroffset;
  int rc;
  pcre* re;
  /* const char* pattern = "(?P<define>define)|(?P<undef>undef)|(?P<space>\\W)"; */
  /* const char* str = " define"; */
  const char* pattern = "(?P<line>[\\r]?\\n)|^(?P<space>[\\t ])*(?P<sharp>#)";
  const char* str = "\n            #"; 
  int i;
  int vector[OVECCOUNT];
  
  re = pcre_compile(
    pattern,              /* the pattern */
    PCRE_MULTILINE,                    /* default options */
    &error,               /* for error message */
    &erroffset,           /* for error offset */
    NULL);                /* use default character tables */

  rc = pcre_exec(
    re,
    NULL,
    str,
    strlen(str),
    0,
    0,
    vector,
    OVECCOUNT);

  if (rc < 0)
  {
    switch(rc)
    {
    case PCRE_ERROR_NOMATCH:
      printf("No match\n");
      break;
    default:
      printf("Matching error %d\n", rc);
      break;
    }
    pcre_free(re);
    return 1;
  }
  printf("\nMatch successed at offset %d\n", vector[0]);
  
  for (i = 0; i < rc; i++)
  {
    const char* sub_start = str + vector[2 * i];
    int sublen = vector[2* i + 1] - vector[2* i];
    printf("%2d: %.*s\n" , i, sublen, sub_start);
  }

  int namecount;
  unsigned char *name_table;
  int name_entry_size;

  (void)pcre_fullinfo(
    re,                   /* the compiled pattern */
    NULL,                 /* no extra data - we didn't study the pattern */
    PCRE_INFO_NAMECOUNT,  /* number of named substrings */
    &namecount);          /* where to put the answer */

  if (namecount <= 0) printf("No named substrings\n"); else
  {
    unsigned char *tabptr;
    printf("Named substrings\n");

    /* Before we can access the substrings, we must extract the table for
       translating names to numbers, and the size of each entry in the table. */

    (void)pcre_fullinfo(
      re,                       /* the compiled pattern */
      NULL,                     /* no extra data - we didn't study the pattern */
      PCRE_INFO_NAMETABLE,      /* address of the table */
      &name_table);             /* where to put the answer */

    (void)pcre_fullinfo(
      re,                       /* the compiled pattern */
      NULL,                     /* no extra data - we didn't study the pattern */
      PCRE_INFO_NAMEENTRYSIZE,  /* size of each entry in the table */
      &name_entry_size);        /* where to put the answer */

    /* Now we can scan the table and, for each entry, print the number, the name,
       and the substring itself. */

    tabptr = name_table;
    for (i = 0; i < namecount; i++)
    {
      int n = (tabptr[0] << 8) | tabptr[1];
      if (vector[2*n + 1] != vector[2*n])
          printf("(%d) %*s: %.*s\n", n, name_entry_size - 3, tabptr + 2,
                 vector[2*n+1] - vector[2*n], str + vector[2*n]);
      tabptr += name_entry_size;
    }
  }

  pcre_free(re);
}
