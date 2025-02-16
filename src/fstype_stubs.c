/* Unison file synchronizer: src/fstype_stubs.c */
/* Copyright 2025, Tõivo Leedjärv

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>
#include <caml/version.h>
#if OCAML_VERSION < 41300
#define CAML_INTERNALS /* was needed from OCaml 4.06 to 4.12 */
#endif
#include <caml/osdeps.h>

#if OCAML_VERSION_MAJOR < 5
#define caml_uerror uerror
#define caml_win32_maperr win32_maperr
#endif


#if defined(__linux)

#include <sys/vfs.h>
#if defined __has_include
#if __has_include (<linux/magic.h>)
#include <linux/magic.h>
#endif
#endif

#ifndef MSDOS_SUPER_MAGIC
#define MSDOS_SUPER_MAGIC 0x4d44
#endif

#ifndef EXFAT_SUPER_MAGIC
#define EXFAT_SUPER_MAGIC 0x2011BAB0
#endif

CAMLprim value unison_get_fstype(value path)
{
  CAMLparam1(path);
  CAMLlocal1(result);
  struct statfs buf;

  if (statfs(String_val(path), &buf) < 0) {
    caml_uerror("statfs", path);
  }

  switch (buf.f_type) {
    case MSDOS_SUPER_MAGIC /* 0x4d44 */:
      result = caml_copy_string("FAT");
      break;
    case EXFAT_SUPER_MAGIC /* 0x2011BAB0 */:
      result = caml_copy_string("exFAT");
      break;
    default:
      result = caml_copy_string("other");
  }

  CAMLreturn(result);
}

#endif // defined(__linux)


#if defined(sun) || defined(__sun) || defined(__NetBSD__)

#if defined(sun) || defined(__sun)
#include <sys/types.h>
#define f_fstypename f_basetype
#endif // defined(sun) || defined(__sun)

#include <sys/statvfs.h>

CAMLprim value unison_get_fstype(value path)
{
  CAMLparam1(path);
  CAMLlocal1(result);
  struct statvfs buf;

  if (statvfs(String_val(path), &buf) < 0) {
    caml_uerror("statvfs", path);
  }

  result = caml_copy_string(buf.f_fstypename);
  CAMLreturn(result);
}

#endif // defined(sun) || defined(__sun) || defined(__NetBSD__)


#if defined(__FreeBSD__) || defined(__APPLE__) || defined(__OpenBSD__) || defined(__DragonFly__)

#if defined(__OpenBSD__)
#include <sys/types.h>
#else
#include <sys/param.h>
#endif
#include <sys/mount.h>

CAMLprim value unison_get_fstype(value path)
{
  CAMLparam1(path);
  CAMLlocal1(result);
  struct statfs buf;

  if (statfs(String_val(path), &buf) < 0) {
    caml_uerror("statfs", path);
  }

  result = caml_copy_string(buf.f_fstypename);
  CAMLreturn(result);
}

#endif // defined(__FreeBSD__) || defined(__APPLE__) || defined(__OpenBSD__) || defined(__DragonFly__)


#if defined(_WIN32)

#ifndef UNICODE
#define UNICODE
#endif
#ifndef _UNICODE
#define _UNICODE
#endif

#include <windows.h>

CAMLprim value unison_get_fstype(value path)
{
  CAMLparam1(path);
  CAMLlocal1(result);
  wchar_t *wpath = caml_stat_strdup_to_utf16(String_val(path));
  wchar_t volpath[1024] = {0};
  wchar_t fstype[128] = {0};
  BOOL res;

  res = GetVolumePathNameW(wpath, volpath, 1024);
  caml_stat_free(wpath);
  if (!res) {
    caml_win32_maperr(GetLastError());
    caml_uerror("get_fstype1", path);
  }

  res = GetVolumeInformationW(volpath, NULL, 0, NULL, NULL, NULL, fstype, 128);
  if (!res) {
    caml_win32_maperr(GetLastError());
    caml_uerror("get_fstype2", caml_copy_string_of_utf16(volpath));
  }

  result = caml_copy_string_of_utf16(fstype);
  /* Will return: FAT, FAT32, exFAT, NTFS, ReFS, among others. */
  CAMLreturn(result);
}

#endif // defined(_WIN32)
