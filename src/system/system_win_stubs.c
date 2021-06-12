#define WINVER 0x0500

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#include <windows.h>
#include <fcntl.h>

#define Nothing ((value) 0)

value copy_wstring(LPCWSTR s)
{
  int len;
  value res;

  len = 2 * wcslen(s) + 2;  /* NULL character included */
  res = caml_alloc_string(len);
  memmove((char *)String_val(res), s, len);
  return res;
}

extern void win32_maperr (DWORD errcode);
extern void uerror (char * cmdname, value arg);

/****/

static HANDLE conin = INVALID_HANDLE_VALUE;

static void init_conin ()
{
  if (conin == INVALID_HANDLE_VALUE) {
    conin = CreateFile ("CONIN$", GENERIC_READ | GENERIC_WRITE,
                        FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
                        OPEN_EXISTING, 0, 0);
    if (conin == INVALID_HANDLE_VALUE) {
      win32_maperr (GetLastError ());
      uerror("init_conin", Nothing);
    }
  }
}

CAMLprim value win_get_console_mode (value unit)
{
  DWORD mode;
  BOOL res;

  init_conin ();

  res = GetConsoleMode (conin, &mode);
  if (res == 0) {
    win32_maperr (GetLastError ());
    uerror("get_console_mode", Nothing);
  }

  return (Val_int (mode));
}

CAMLprim value win_set_console_mode (value mode)
{
  BOOL res;

  init_conin ();

  res = SetConsoleMode (conin, Int_val(mode));
  if (res == 0) {
    win32_maperr (GetLastError ());
    uerror("set_console_mode", Nothing);
  }
  return (Val_unit);
}

CAMLprim value win_get_console_output_cp (value unit) {
  return (Val_int (GetConsoleOutputCP ()));
}

CAMLprim value win_set_console_output_cp (value cp) {
  BOOL res;
  res = SetConsoleOutputCP (Int_val (cp));
  if (res == 0) {
    win32_maperr (GetLastError ());
    uerror("set_console_cp", Nothing);
  }
  return (Val_unit);
}
