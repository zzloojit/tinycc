D             [0-9]
L             [a-zA-Z_]
H             [a-fA-F0-9]

%{
#include <stdio.h>
#include "cpptok.h"

%}

%%

" "        {return (TOK_SPACE);   }
"#include" {return (TOK_INCLUDE); }
"#define"  {return (TOK_DEFINE);  }
"#undef"   {return (TOK_UNDEF);   }

