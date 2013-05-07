
#define print_line \
  fprintf(s->ppfp, "# %d \"%s\"\n", file->line_num,     \
          file->filename)

int preprocess()
{
  for(;;) 
  {
    next();
    if (tok == TOK_EOF) 
    {
      break;
    }
    else if (file != file_ref) 
    {
      print_line;
    } 
    else if (tok == TOK_LINEFEED) 
    {
      if (!token_seen)
        continue;
      ++line_ref;
      token_seen = 0;
    } 
    else if (!token_seen) 
    {
      d = file->line_num - line_ref;
      if (file != file_ref || d < 0 || d >= 8)
      {
        print_line;
      }
      else
      {
        while (d--)
          fputs("\n", s->ppfp);
      }
      line_ref = (file_ref = file)->line_num;
      token_seen = tok != TOK_LINEFEED;
      if (!token_seen)
        continue;
    }
    fputs(get_tok_str(tok, &tokc), s->ppfp);
  }
}
