/* Unison file synchronizer: src/props_xattr.c */
/* Copyright 2020-2021, Tõivo Leedjärv

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

/* Conceptually, here, an extended attribute is just a name-value pair,
 * where name is a text string and value is a binary string.
 *
 * The external interface is defined as follows. Every supported platform
 * must implement this interface. xattr format can be platform-specific,
 * which may prevent cross-platform synchronization but still allows
 * synchronization within the platform. Cross-platform synchronization may
 * still be possible in some cases, even if one platform will not
 * understand the xattrs; the attribute values are treated as blobs then.
 *
 *
 * SET the value of one xattr
 * ==========================
 * unit unison_xattr_set(String path, (String * Bytes) xattr)
 *
 *   Create the requested extended attribute on the requested file or
 *   directory and set the attribute value.
 *   If the attribute already exists then its value is overwritten.
 *   Symbolic links are not followed. In other words, the xattr is set
 *   on the symbolic link itsel.
 *
 * Input parameters
 *   path  - absolute path of a file or directory
 *   xattr - name-value pair of attribute to set on the path
 *
 * Return value
 *   No return value.
 *
 * Exceptions
 *   There are no mandatory exception conditions.
 *   OCaml exception defined by macro UNSN_XATTR_NOT_SUPPORTED_EX
 *   MAY be raised when extended attributes are not supported on
 *   the requested path.
 *   Failure MAY voluntarily be raised for example when:
 *     Can't access file to set the attribute
 *     Error creating the attribute (invalid name, permission error, etc.)
 *     Error setting the attribute value
 *
 *
 * REMOVE one xattr
 * ================
 * unit unison_xattr_remove(String path, String xattrname)
 *
 *   Remove the requested extended attribute on the requested file or
 *   directory.
 *
 * Input parameters
 *   path      - absolute path of a file or directory
 *   xattrname - name of attribute to remove on the path
 *
 * Return value
 *   No return value.
 *
 * Exceptions
 *   There are no mandatory exception conditions.
 *   OCaml exception defined by macro UNSN_XATTR_NOT_SUPPORTED_EX
 *   MAY be raised when extended attributes are not supported on
 *   the requested path.
 *   Failure MAY voluntarily be raised for example when:
 *     Can't access file to remove the attribute
 *     Error removing the attribute
 *
 *
 * GET the list of xattrs with values
 * ==================================
 * List of (String * Bytes) unison_xattrs_get(String path)
 *
 *   Get the list of all extended attributes on the requested file or
 *   directory. Attributes are returned together with their values.
 *   Attributes in the list can be returned in any order and the order
 *   does not have to be stable (i.e. it can be different on every
 *   invocation on the same path).
 *   For every attribute value, at most UNSN_MAX_XATTR_VALUE_SIZE bytes
 *   must be returned, even if it means that the value will be truncated.
 *
 * Input parameters
 *   path - absolute path of a file or directory
 *
 * Return value
 *   The list of name-value pairs, with each pair representing the
 *   name and value of one extended attribute.
 *
 * Exceptions
 *   OCaml exception defined by macro UNSN_XATTR_NOT_SUPPORTED_EX
 *   MUST be raised when extended attributes are not supported on
 *   the requested path, or should not otherwise be returned.
 *   Failure MAY voluntarily be raised for example when:
 *     Can't access file to get the attributes
 *     Error reading attribute values
 *
 */

#define CAML_NAME_SPACE
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>


#if defined(sun) || defined(__sun)  /* Solarish, all illumos-based OS,   */
#define __Solaris__                 /* OpenIndiana, OmniOS, SmartOS, ... */
#endif

#undef UNSN_HAS_XATTR
#if defined(__Solaris__) || defined(__FreeBSD__) || defined(__APPLE__) || defined(__linux)
#define UNSN_HAS_XATTR
#endif

#define UNSN_MAX_XATTR_VALUE_SIZE 383

#define UNSN_XATTR_NOT_SUPPORTED_EX "XattrNotSupported"


static void unsn_xattr_not_supported()
{
  static const value *ex = NULL;

  if (ex == NULL) {
    ex = caml_named_value(UNSN_XATTR_NOT_SUPPORTED_EX);
  }

  caml_raise_constant(*ex);
}


#ifndef UNSN_HAS_XATTR

CAMLprim void unison_xattr_set(value path, value xattr)
{
  CAMLparam2(path, xattr);
  unsn_xattr_not_supported();
}

CAMLprim void unison_xattr_remove(value path, value xattrname)
{
  CAMLparam2(path, xattrname);
  unsn_xattr_not_supported();
}

CAMLprim value unison_xattrs_get(value path)
{
  CAMLparam1(path);
  unsn_xattr_not_supported();
}

#else /* UNSN_HAS_XATTR */


#if defined(__Solaris__)
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <unistd.h>
#include <string.h>
#endif

#if defined(__FreeBSD__)
#include <errno.h>
#include <sys/types.h>
#include <sys/extattr.h>
#include <string.h>

#define ENOTSUP EOPNOTSUPP
#endif

#if defined(__APPLE__)
#include <errno.h>
#include <sys/xattr.h>
#include <string.h>
#endif

#if defined(__linux)
#include <errno.h>
#include <sys/types.h>
#include <sys/xattr.h>
#include <string.h>
#endif


/* FIXME: Remove when support for OCaml < 4.06 is dropped */
#ifndef Bytes_val /* OCaml < 4.06 */
/* Copied from OCaml byterun/alloc.c; LGPL 2.1 */
static value caml_alloc_initialized_string (mlsize_t len, const char *p)
{
  value result = caml_alloc_string (len);
  memcpy((char *)String_val(result), p, len);
  return result;
}
#endif


static int unsn_is_system_attr_os(const char *attrname)
{
#if defined(__linux)
  return (strncmp(attrname, "system.", 7) == 0);
#elif defined(__APPLE__)
  return (strcmp(attrname, XATTR_FINDERINFO_NAME) == 0 ||
          strcmp(attrname, XATTR_RESOURCEFORK_NAME) == 0);
#elif defined(__FreeBSD__)
  return 0;
#elif defined(__Solaris__)
  /* Special system "extensible attributes" xattrs are defined in sys/attr.h
   * as VIEW_READONLY = "SUNWattr_ro" and VIEW_READWRITE = "SUNWattr_rw" */
  return (strcmp(attrname, ".") == 0 || strcmp(attrname, "..") == 0 ||
          strncmp(attrname, "SUNWattr_", 9) == 0);
#endif
}


/************************************
 *            Set xattr
 ************************************/
static int unsn_set_xattr_os(const char *path, const char *attrname,
                             const void *attrvalue, size_t valuesize)
{
#if defined(__linux)
  return lsetxattr(path, attrname, attrvalue, valuesize, 0);
#elif defined(__APPLE__)
  return setxattr(path, attrname, attrvalue, valuesize, 0, XATTR_NOFOLLOW);
#elif defined(__FreeBSD__)
  return (int) extattr_set_link(path, EXTATTR_NAMESPACE_USER, attrname,
                                attrvalue, valuesize);
#elif defined(__Solaris__)
  if (pathconf(path, _PC_XATTR_ENABLED) < 1) {
    unsn_xattr_not_supported();
  }

  /* This is a simplified implementation that just creates/opens
   * the xattr and writes the value into it.
   *
   * Extended attributes in Solaris and illumos are much more
   * flexible. In most ways they are like normal files/directories.
   * They have owner/group, mode, utimes, even ACL, and can have
   * their own extended attributes, etc.
   *
   * This implementation does not synchronize any of those params,
   * as xattrs are conceptually treated as name-value pairs.
   * It is unknown if this will cause problems with real use cases. */
  int fd = attropen(path, attrname, O_CREAT|O_WRONLY|O_TRUNC);
  if (fd == -1) {
    caml_failwith("Error opening extended attribute for writing");
  }

  ssize_t count = write(fd, attrvalue, valuesize);
  if (count == -1) {
    caml_failwith("Error writing extended attribute value");
  }

  close(fd);

  return 0;
#endif
}

CAMLprim value unison_xattr_set(value path, value xattr)
{
  CAMLparam2(path, xattr);
  const char *name = String_val(path);
  const char *attr;
  unsigned char *attrvalue;
  unsigned int len;

  if (!Is_block(xattr)) {
    caml_failwith("Internal error: Invalid xattr data");
  }

  attr = String_val(Field(xattr, 0));

  /* Ignore system extended attributes */
  if (unsn_is_system_attr_os(attr)) {
    CAMLreturn(Val_unit);
  }

  attrvalue = Bytes_val(Field(xattr, 1));
  len = caml_string_length(Field(xattr, 1));

  int error = unsn_set_xattr_os(name, attr, attrvalue, len);
  if (error == -1) {
    if (errno == ENOTSUP) {
      unsn_xattr_not_supported();
    } else {
      caml_failwith("Error setting extended attribute value");
    }
  }

  CAMLreturn(Val_unit);
}


/************************************
 *           Remove xattr
 ************************************/
static int unsn_remove_xattr_os(const char *path, const char *attrname)
{
#if defined(__linux)
  return lremovexattr(path, attrname);
#elif defined(__APPLE__)
  return removexattr(path, attrname, XATTR_NOFOLLOW);
#elif defined(__FreeBSD__)
  return (int) extattr_delete_link(path, EXTATTR_NAMESPACE_USER, attrname);
#elif defined(__Solaris__)
  if (pathconf(path, _PC_XATTR_ENABLED) < 1) {
    unsn_xattr_not_supported();
  }

  int fd = attropen(path, ".", O_RDONLY);
  if (fd == -1) {
    caml_failwith("Error opening extended attribute for removing");
  }
  int error = unlinkat(fd, attrname, 0);
  /* TODO: check for errors? ENOENT perhaps? What about directories? rmdir */
  close(fd);

  return error;
#endif
}

CAMLprim value unison_xattr_remove(value path, value xattrname)
{
  CAMLparam2(path, xattrname);
  const char *name = String_val(path);
  const char *attr = String_val(xattrname);

  /* Ignore system extended attributes */
  if (unsn_is_system_attr_os(attr)) {
    CAMLreturn(Val_unit);
  }

  int error = unsn_remove_xattr_os(name, attr);
  if (error == -1 && errno == ENOTSUP) {
    unsn_xattr_not_supported();
  }

  CAMLreturn(Val_unit);
}


/************************************
 *            Get xattrs
 ************************************/
static ssize_t unsn_get_xattr_os(const char *path, const char *attrname,
                                 void *buf, size_t size)
{
#if defined(__linux)
  return lgetxattr(path, attrname, buf, size);
#elif defined(__APPLE__)
  return getxattr(path, attrname, buf, size, 0, XATTR_NOFOLLOW);
#elif defined(__FreeBSD__)
  return extattr_get_link(path, EXTATTR_NAMESPACE_USER, attrname, buf, size);
#elif defined(__Solaris__)
  int attrfd = attropen(path, attrname, O_RDONLY);
  if (attrfd == -1) {
    caml_failwith("Error opening extended attribute for reading");
  }

  ssize_t r = read(attrfd, buf, size);
  close(attrfd);

  return r;
#endif
}

#if !defined(__Solaris__)

static ssize_t unsn_list_xattr_os(const char *path, char *buf, size_t size)
{
#if defined(__linux)
  return llistxattr(path, buf, size);
#elif defined(__APPLE__)
  return listxattr(path, buf, size, XATTR_NOFOLLOW);
#elif defined(__FreeBSD__)
  return extattr_list_link(path, EXTATTR_NAMESPACE_USER, buf, size);
#endif
}

static ssize_t unsn_list_xattr_aux(const char *path, char **buf)
{
  ssize_t namelen;

  namelen = unsn_list_xattr_os(path, NULL, 0);

  if (namelen == -1) {
    if (errno == ENOTSUP) {
      unsn_xattr_not_supported();
    }
    caml_failwith("Error getting list of extended attributes");
  }
  if (namelen == 0) {
    return 0;
  }

  *buf = malloc(namelen);
  if (buf == NULL) {
    caml_failwith("Error getting list of extended attributes");
  }

  namelen = unsn_list_xattr_os(path, *buf, namelen);
  if (namelen == -1) {
    free(*buf);
    caml_failwith("Error getting list of extended attributes");
  }
  if (namelen == 0) {
    free(*buf);
    return 0;
  }

  return namelen;
}

#else

static ssize_t unsn_list_xattr_aux(const char *path, DIR **dirp)
{
  if (pathconf(path, _PC_XATTR_ENABLED) < 1) {
    unsn_xattr_not_supported();
  }

  if (pathconf(path, _PC_XATTR_EXISTS) < 1) {
    return 0;
  }

  int fd = attropen(path, ".", O_RDONLY);
  if (fd == -1) {
    caml_failwith("Error getting list of extended attributes");
  }

  *dirp = fdopendir(fd);
  if (*dirp == NULL) {
    caml_failwith("Error getting list of extended attributes");
  }

  return 1;
}

#endif /* !__Solaris__ */

CAMLprim value unison_xattrs_get(value path)
{
  CAMLparam1(path);
  CAMLlocal3(result, p, l);
  const char *name = String_val(path);
#if !defined(__Solaris__)
  char *xattrs;
#else
  DIR *xattrs;
  struct dirent *dp;
  char *xattrname;
#endif
  ssize_t namelen, len;
  char buf[UNSN_MAX_XATTR_VALUE_SIZE];

  result = Val_emptylist;

  namelen = unsn_list_xattr_aux(name, &xattrs);
  if (namelen == 0) {
    CAMLreturn(result);
  }

#if defined(__FreeBSD__)
  size_t nl = 0;
  char xattrname[256];

  for (char *xattrnamep = xattrs; xattrnamep < xattrs + namelen;
                                  xattrnamep += nl + 1) {
    nl = *xattrnamep & 255;
    memcpy(xattrname, xattrnamep + 1, nl);
    xattrname[nl] = '\0';
#elif !defined(__Solaris__)
  /* For safety */
  *(xattrs + namelen - 1) = '\0';

  for (char *xattrname = xattrs; xattrname < xattrs + namelen;
                                 xattrname += strlen(xattrname) + 1) {
#elif defined(__Solaris__)
  while (dp = readdir(xattrs)) {
    /* Note: NULL is returned for both end of dir and an error condition.
     * Error conditions are silently ignored. */
    xattrname = dp->d_name;
#endif

    /* Ignore system extended attributes */
    if (unsn_is_system_attr_os(xattrname)) {
      continue;
    }

    len = unsn_get_xattr_os(name, xattrname, buf, UNSN_MAX_XATTR_VALUE_SIZE);
    if (len == -1) {
      continue; /* Ignore silently */
    }

    p = caml_alloc_tuple(2);
    Store_field(p, 0, caml_copy_string(xattrname));
    Store_field(p, 1, caml_alloc_initialized_string(len, buf));

    l = caml_alloc_small(2, Tag_cons);
    Field(l, 0) = p;
    Field(l, 1) = result;

    result = l;
  }

#if defined(__Solaris__)
  closedir(xattrs);
#else
  free(xattrs);
#endif

  CAMLreturn(result);
}


#endif /* UNSN_HAS_XATTR */
