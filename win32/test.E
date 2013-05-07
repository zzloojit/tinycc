# 1 "e:/github/tinycc/win32/include/stdio.h" 1




 
# 1 "e:/github/tinycc/win32/include/_mingw.h" 1
# 16 "e:/github/tinycc/win32/include/_mingw.h" 3
 




 



 
# 1 "e:/github/tinycc/win32/include/stddef.h" 1



typedef unsigned long size_t;
typedef long ssize_t;
typedef unsigned short wchar_t;
typedef long ptrdiff_t;
typedef long intptr_t;
typedef unsigned long uintptr_t;



typedef signed char int8_t;
typedef signed short int int16_t;
typedef signed int int32_t;
typedef signed long long int int64_t;
typedef unsigned char uint8_t;
typedef unsigned short int uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long long int uint64_t;





void *alloca(size_t size);
# 28 "e:/github/tinycc/win32/include/_mingw.h" 2
# 1 "e:/github/tinycc/win32/include/stdarg.h" 1
# 29 "e:/github/tinycc/win32/include/stdarg.h" 3
typedef char *va_list;
 






 
typedef va_list __gnuc_va_list;
# 29 "e:/github/tinycc/win32/include/_mingw.h" 2
# 81 "e:/github/tinycc/win32/include/_mingw.h" 3
 
# 91 "e:/github/tinycc/win32/include/_mingw.h" 3
typedef long __time32_t;

typedef long long __time64_t;


typedef __time32_t time_t;






typedef unsigned long size_t;

typedef long ssize_t;


typedef unsigned int wint_t;
typedef unsigned short wctype_t;

typedef unsigned short wchar_t;


typedef int errno_t;


typedef struct threadlocaleinfostruct *pthreadlocinfo;
typedef struct threadmbcinfostruct *pthreadmbcinfo;
typedef struct localeinfo_struct _locale_tstruct,*_locale_t;

 
# 10 "e:/github/tinycc/win32/include/stdio.h" 2
# 24 "e:/github/tinycc/win32/include/stdio.h" 3
  struct _iobuf {
    char *_ptr;
    int _cnt;
    char *_base;
    int _flag;
    int _file;
    int _charbuf;
    int _bufsiz;
    char *_tmpfname;
  };
  typedef struct _iobuf FILE;
# 78 "e:/github/tinycc/win32/include/stdio.h" 3
  typedef long _off_t;

  typedef long off_t;






  typedef long long _off64_t;

  typedef long long off64_t;
# 101 "e:/github/tinycc/win32/include/stdio.h" 3
extern FILE (*_imp___iob)[];     
# 113 "e:/github/tinycc/win32/include/stdio.h" 3
  typedef long long fpos_t;
# 150 "e:/github/tinycc/win32/include/stdio.h" 3
  extern int __attribute__((__cdecl__)) _filbuf(FILE *_File);
  extern int __attribute__((__cdecl__)) _flsbuf(int _Ch,FILE *_File);



  extern FILE *__attribute__((__cdecl__)) _fsopen(const char *_Filename,const char *_Mode,int _ShFlag);

  void __attribute__((__cdecl__)) clearerr(FILE *_File);
  int __attribute__((__cdecl__)) fclose(FILE *_File);
  extern int __attribute__((__cdecl__)) _fcloseall(void);



  extern FILE *__attribute__((__cdecl__)) _fdopen(int _FileHandle,const char *_Mode);

  int __attribute__((__cdecl__)) feof(FILE *_File);
  int __attribute__((__cdecl__)) ferror(FILE *_File);
  int __attribute__((__cdecl__)) fflush(FILE *_File);
  int __attribute__((__cdecl__)) fgetc(FILE *_File);
  extern int __attribute__((__cdecl__)) _fgetchar(void);
  int __attribute__((__cdecl__)) fgetpos(FILE *_File ,fpos_t *_Pos);
  char *__attribute__((__cdecl__)) fgets(char *_Buf,int _MaxCount,FILE *_File);



  extern int __attribute__((__cdecl__)) _fileno(FILE *_File);

  extern char *__attribute__((__cdecl__)) _tempnam(const char *_DirName,const char *_FilePrefix);
  extern int __attribute__((__cdecl__)) _flushall(void);
  FILE *__attribute__((__cdecl__)) fopen(const char *_Filename,const char *_Mode);
  FILE *fopen64(const char *filename,const char *mode);
  int __attribute__((__cdecl__)) fprintf(FILE *_File,const char *_Format,...);
  int __attribute__((__cdecl__)) fputc(int _Ch,FILE *_File);
  extern int __attribute__((__cdecl__)) _fputchar(int _Ch);
  int __attribute__((__cdecl__)) fputs(const char *_Str,FILE *_File);
  size_t __attribute__((__cdecl__)) fread(void *_DstBuf,size_t _ElementSize,size_t _Count,FILE *_File);
  FILE *__attribute__((__cdecl__)) freopen(const char *_Filename,const char *_Mode,FILE *_File);
  int __attribute__((__cdecl__)) fscanf(FILE *_File,const char *_Format,...);
  int __attribute__((__cdecl__)) fsetpos(FILE *_File,const fpos_t *_Pos);
  int __attribute__((__cdecl__)) fseek(FILE *_File,long _Offset,int _Origin);
   int fseeko64(FILE* stream, _off64_t offset, int whence);
  long __attribute__((__cdecl__)) ftell(FILE *_File);
  _off64_t ftello64(FILE * stream);
  int __attribute__((__cdecl__)) _fseeki64(FILE *_File,long long _Offset,int _Origin);
  long long __attribute__((__cdecl__)) _ftelli64(FILE *_File);
  size_t __attribute__((__cdecl__)) fwrite(const void *_Str,size_t _Size,size_t _Count,FILE *_File);
  int __attribute__((__cdecl__)) getc(FILE *_File);
  int __attribute__((__cdecl__)) getchar(void);
  extern int __attribute__((__cdecl__)) _getmaxstdio(void);
  char *__attribute__((__cdecl__)) gets(char *_Buffer);
  int __attribute__((__cdecl__)) _getw(FILE *_File);


  void __attribute__((__cdecl__)) perror(const char *_ErrMsg);

  extern int __attribute__((__cdecl__)) _pclose(FILE *_File);
  extern FILE *__attribute__((__cdecl__)) _popen(const char *_Command,const char *_Mode);




  int __attribute__((__cdecl__)) printf(const char *_Format,...);
  int __attribute__((__cdecl__)) putc(int _Ch,FILE *_File);
  int __attribute__((__cdecl__)) putchar(int _Ch);
  int __attribute__((__cdecl__)) puts(const char *_Str);
  extern int __attribute__((__cdecl__)) _putw(int _Word,FILE *_File);


  int __attribute__((__cdecl__)) remove(const char *_Filename);
  int __attribute__((__cdecl__)) rename(const char *_OldFilename,const char *_NewFilename);
  extern int __attribute__((__cdecl__)) _unlink(const char *_Filename);

  int __attribute__((__cdecl__)) unlink(const char *_Filename);


  void __attribute__((__cdecl__)) rewind(FILE *_File);
  extern int __attribute__((__cdecl__)) _rmtmp(void);
  int __attribute__((__cdecl__)) scanf(const char *_Format,...);
  void __attribute__((__cdecl__)) setbuf(FILE *_File,char *_Buffer);
  extern int __attribute__((__cdecl__)) _setmaxstdio(int _Max);
  extern unsigned int __attribute__((__cdecl__)) _set_output_format(unsigned int _Format);
  extern unsigned int __attribute__((__cdecl__)) _get_output_format(void);
  int __attribute__((__cdecl__)) setvbuf(FILE *_File,char *_Buf,int _Mode,size_t _Size);
  extern int __attribute__((__cdecl__)) _scprintf(const char *_Format,...);
  int __attribute__((__cdecl__)) sscanf(const char *_Src,const char *_Format,...);
  extern int __attribute__((__cdecl__)) _snscanf(const char *_Src,size_t _MaxCount,const char *_Format,...);
  FILE *__attribute__((__cdecl__)) tmpfile(void);
  char *__attribute__((__cdecl__)) tmpnam(char *_Buffer);
  int __attribute__((__cdecl__)) ungetc(int _Ch,FILE *_File);
  int __attribute__((__cdecl__)) vfprintf(FILE *_File,const char *_Format,va_list _ArgList);
  int __attribute__((__cdecl__)) vprintf(const char *_Format,va_list _ArgList);
   


  
  
  extern
  __attribute__((format(gnu_printf, 3, 0))) __attribute__((nonnull (3)))
  int __mingw_vsnprintf(char *_DstBuf,size_t _MaxCount,const char *_Format,va_list _ArgList);
  extern
  __attribute__((format(gnu_printf, 3, 4))) __attribute__((nonnull (3)))
  int __mingw_snprintf(char* s, size_t n, const char*  format, ...);
  int __attribute__((__cdecl__)) vsnprintf(char *_DstBuf,size_t _MaxCount,const char *_Format,va_list _ArgList);
  extern int __attribute__((__cdecl__)) _snprintf(char *_Dest,size_t _Count,const char *_Format,...);
  extern int __attribute__((__cdecl__)) _vsnprintf(char *_Dest,size_t _Count,const char *_Format,va_list _Args);
  int __attribute__((__cdecl__)) sprintf(char *_Dest,const char *_Format,...);
  int __attribute__((__cdecl__)) vsprintf(char *_Dest,const char *_Format,va_list _Args);

  int __attribute__((__cdecl__)) snprintf(char* s, size_t n, const char*  format, ...);
  extern __inline__ int __attribute__((__cdecl__)) vsnprintf (char* s, size_t n, const char* format,va_list arg) {
    return _vsnprintf ( s, n, format, arg);
  }
  int __attribute__((__cdecl__)) vscanf(const char * Format, va_list argp);
  int __attribute__((__cdecl__)) vfscanf (FILE * fp, const char * Format,va_list argp);
  int __attribute__((__cdecl__)) vsscanf (const char * _Str,const char * Format,va_list argp);

 


 
  







  extern int __attribute__((__cdecl__)) _vscprintf(const char *_Format,va_list _ArgList);
  extern int __attribute__((__cdecl__)) _set_printf_count_output(int _Value);
  extern int __attribute__((__cdecl__)) _get_printf_count_output(void);
# 291 "e:/github/tinycc/win32/include/stdio.h" 3
  extern FILE *__attribute__((__cdecl__)) _wfsopen(const wchar_t *_Filename,const wchar_t *_Mode,int _ShFlag);

  wint_t __attribute__((__cdecl__)) fgetwc(FILE *_File);
  extern wint_t __attribute__((__cdecl__)) _fgetwchar(void);
  wint_t __attribute__((__cdecl__)) fputwc(wchar_t _Ch,FILE *_File);
  extern wint_t __attribute__((__cdecl__)) _fputwchar(wchar_t _Ch);
  wint_t __attribute__((__cdecl__)) getwc(FILE *_File);
  wint_t __attribute__((__cdecl__)) getwchar(void);
  wint_t __attribute__((__cdecl__)) putwc(wchar_t _Ch,FILE *_File);
  wint_t __attribute__((__cdecl__)) putwchar(wchar_t _Ch);
  wint_t __attribute__((__cdecl__)) ungetwc(wint_t _Ch,FILE *_File);
  wchar_t *__attribute__((__cdecl__)) fgetws(wchar_t *_Dst,int _SizeInWords,FILE *_File);
  int __attribute__((__cdecl__)) fputws(const wchar_t *_Str,FILE *_File);
  extern wchar_t *__attribute__((__cdecl__)) _getws(wchar_t *_String);
  extern int __attribute__((__cdecl__)) _putws(const wchar_t *_Str);
  int __attribute__((__cdecl__)) fwprintf(FILE *_File,const wchar_t *_Format,...);
  int __attribute__((__cdecl__)) wprintf(const wchar_t *_Format,...);
  extern int __attribute__((__cdecl__)) _scwprintf(const wchar_t *_Format,...);
  int __attribute__((__cdecl__)) vfwprintf(FILE *_File,const wchar_t *_Format,va_list _ArgList);
  int __attribute__((__cdecl__)) vwprintf(const wchar_t *_Format,va_list _ArgList);
  extern int __attribute__((__cdecl__)) swprintf(wchar_t*, const wchar_t*, ...);
  extern int __attribute__((__cdecl__)) vswprintf(wchar_t*, const wchar_t*,va_list);
  extern int __attribute__((__cdecl__)) _swprintf_c(wchar_t *_DstBuf,size_t _SizeInWords,const wchar_t *_Format,...);
  extern int __attribute__((__cdecl__)) _vswprintf_c(wchar_t *_DstBuf,size_t _SizeInWords,const wchar_t *_Format,va_list _ArgList);
  extern int __attribute__((__cdecl__)) _snwprintf(wchar_t *_Dest,size_t _Count,const wchar_t *_Format,...);
  extern int __attribute__((__cdecl__)) _vsnwprintf(wchar_t *_Dest,size_t _Count,const wchar_t *_Format,va_list _Args);

  int __attribute__((__cdecl__)) snwprintf (wchar_t* s, size_t n, const wchar_t*  format, ...);
  extern __inline__ int __attribute__((__cdecl__)) vsnwprintf (wchar_t* s, size_t n, const wchar_t* format, va_list arg) { return _vsnwprintf(s,n,format,arg); }
  int __attribute__((__cdecl__)) vwscanf (const wchar_t *, va_list);
  int __attribute__((__cdecl__)) vfwscanf (FILE *,const wchar_t *,va_list);
  int __attribute__((__cdecl__)) vswscanf (const wchar_t *,const wchar_t *,va_list);

  extern int __attribute__((__cdecl__)) _swprintf(wchar_t *_Dest,const wchar_t *_Format,...);
  extern int __attribute__((__cdecl__)) _vswprintf(wchar_t *_Dest,const wchar_t *_Format,va_list _Args);
# 1 "e:/github/tinycc/win32/include/vadefs.h" 1




 



 
# 329 "e:/github/tinycc/win32/include/stdio.h" 2
# 340 "e:/github/tinycc/win32/include/stdio.h" 3
  extern wchar_t *__attribute__((__cdecl__)) _wtempnam(const wchar_t *_Directory,const wchar_t *_FilePrefix);
  extern int __attribute__((__cdecl__)) _vscwprintf(const wchar_t *_Format,va_list _ArgList);
  int __attribute__((__cdecl__)) fwscanf(FILE *_File,const wchar_t *_Format,...);
  int __attribute__((__cdecl__)) swscanf(const wchar_t *_Src,const wchar_t *_Format,...);
  extern int __attribute__((__cdecl__)) _snwscanf(const wchar_t *_Src,size_t _MaxCount,const wchar_t *_Format,...);
  int __attribute__((__cdecl__)) wscanf(const wchar_t *_Format,...);
  extern FILE *__attribute__((__cdecl__)) _wfdopen(int _FileHandle ,const wchar_t *_Mode);
  extern FILE *__attribute__((__cdecl__)) _wfopen(const wchar_t *_Filename,const wchar_t *_Mode);
  extern FILE *__attribute__((__cdecl__)) _wfreopen(const wchar_t *_Filename,const wchar_t *_Mode,FILE *_OldFile);


  extern void __attribute__((__cdecl__)) _wperror(const wchar_t *_ErrMsg);

  extern FILE *__attribute__((__cdecl__)) _wpopen(const wchar_t *_Command,const wchar_t *_Mode);



  extern int __attribute__((__cdecl__)) _wremove(const wchar_t *_Filename);
  extern wchar_t *__attribute__((__cdecl__)) _wtmpnam(wchar_t *_Buffer);
  extern wint_t __attribute__((__cdecl__)) _fgetwc_nolock(FILE *_File);
  extern wint_t __attribute__((__cdecl__)) _fputwc_nolock(wchar_t _Ch,FILE *_File);
  extern wint_t __attribute__((__cdecl__)) _ungetwc_nolock(wint_t _Ch,FILE *_File);
# 393 "e:/github/tinycc/win32/include/stdio.h" 3
  extern void __attribute__((__cdecl__)) _lock_file(FILE *_File);
  extern void __attribute__((__cdecl__)) _unlock_file(FILE *_File);
  extern int __attribute__((__cdecl__)) _fclose_nolock(FILE *_File);
  extern int __attribute__((__cdecl__)) _fflush_nolock(FILE *_File);
  extern size_t __attribute__((__cdecl__)) _fread_nolock(void *_DstBuf,size_t _ElementSize,size_t _Count,FILE *_File);
  extern int __attribute__((__cdecl__)) _fseek_nolock(FILE *_File,long _Offset,int _Origin);
  extern long __attribute__((__cdecl__)) _ftell_nolock(FILE *_File);
  extern int __attribute__((__cdecl__)) _fseeki64_nolock(FILE *_File,long long _Offset,int _Origin);
  extern long long __attribute__((__cdecl__)) _ftelli64_nolock(FILE *_File);
  extern size_t __attribute__((__cdecl__)) _fwrite_nolock(const void *_DstBuf,size_t _Size,size_t _Count,FILE *_File);
  extern int __attribute__((__cdecl__)) _ungetc_nolock(int _Ch,FILE *_File);





  char *__attribute__((__cdecl__)) tempnam(const char *_Directory,const char *_FilePrefix);
  int __attribute__((__cdecl__)) fcloseall(void);
  FILE *__attribute__((__cdecl__)) fdopen(int _FileHandle,const char *_Format);
  int __attribute__((__cdecl__)) fgetchar(void);
  int __attribute__((__cdecl__)) fileno(FILE *_File);
  int __attribute__((__cdecl__)) flushall(void);
  int __attribute__((__cdecl__)) fputchar(int _Ch);
  int __attribute__((__cdecl__)) getw(FILE *_File);
  int __attribute__((__cdecl__)) putw(int _Ch,FILE *_File);
  int __attribute__((__cdecl__)) rmtmp(void);
# 1 "e:/github/tinycc/win32/include/sec_api/stdio_s.h" 1




 
# 1 "e:/github/tinycc/win32/include/stdio.h" 1




 
# 10 "e:/github/tinycc/win32/include/sec_api/stdio_s.h" 2
# 428 "e:/github/tinycc/win32/include/stdio.h" 2
# 2 "../test.c" 2

int main()
{
  printf("hello world");
}
