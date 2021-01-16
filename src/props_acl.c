/* Unison file synchronizer: src/props_acl.c */
/* Copyright 2020, Tõivo Leedjärv

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

/* Supporting POSIX draft ACLs is not a goal, but may incidentally work
 * on some platforms. Only NFSv4 ACLs and eventually Windows ACLs are
 * intended to be supported.
 *
 * On Solarish both NFSv4 ACLs and POSIX draft ACLs are supported.
 * There is even support for cross-synchronizing between NFSv4 and
 * POSIX draft ACLs, but this support is currently disabled in props.ml
 * by checking if the resulting ACL matches the requested ACL (the check
 * fails with cross-synchronization).
 *
 * On FreeBSD and NetBSD, NFSv4 ACLs are supported. There is only limited
 * support for synchronizing POSIX draft ACLs (no default ACLs).
 *
 * On Darwin, extended ACLs are supported.
 *
 * Currently there is no support for Windows NTFS ACLs. Theoretically
 * this can be added as the interface towards synchronization logic is
 * just a text representation of the ACL. The only requirement is that
 * the string representation must be deterministic and stable. This
 * should be possible either with SDDL format or a custom format. */

/* The external interface is defined as follows. Every supported platform
 * must implement this interface. ACL format can be platform-specific,
 * which will prevent cross-platform synchronization but still allows
 * synchronization within the platform.
 *
 *
 * SET the ACL
 * ===========
 * unit unison_acl_from_text(String path, String acl)
 *
 *   Set the requested ACL on the requested file or directory. The ACL
 *   must be in the same format as that returned by unison_acl_to_text().
 *   NULL or empty string ACL means <no ACL> and results in removal of
 *   any existing ACL on the requested file or directory.
 *   For security reasons, symbolic links are not followed when setting
 *   or removing ACLs.
 *
 * Input parameters
 *   path - absolute path of a file or directory
 *   acl  - text representation of ACL to set on the path
 *
 * Return value
 *   No return value.
 *
 * Exceptions
 *   There are no mandatory exception conditions.
 *   Failure MAY voluntarily be raised for example when:
 *     Can't access file to set/remove ACL
 *     ACL not supported
 *     Error setting ACL
 *     Error removing ACL
 *     Error converting ACL from text
 *
 *
 * GET the ACL
 * ===========
 * String unison_acl_to_text(String path)
 *
 *   Get the current ACL on the requested file or directory. The ACL
 *   must be returned as a stable and deterministic text representation
 *   that meets the following criteria:
 *     - with multiple requests on the same file, the representation is
 *       always the same, unless the underlying ACL changes;
 *     - the same ACL on different files has the same representation.
 *
 * Input parameters
 *   path - absolute path of a file or directory
 *
 * Return value
 *   The text representation of the ACL;
 *   or empty string "" meaning <no ACL> (or only trivial ACL)
 *   or the value of UNSN_ACL_NOT_SUPPORTED (currently "-1") if ACL is
 *   not supported on the requested path.
 *
 * Exceptions
 *   There are no mandatory exception conditions.
 *
 *
 * ===========
 * Definition of ACL format
 *
 * The format of ACL text representation is completely free as long as
 * following constraints are met:
 *   - output of unison_acl_to_text() can be used as
 *     input to unison_acl_from_text()
 *   - ACL synchronization is done only on the same platform.
 *
 * If ACLs must be synchronized between different platforms then the
 * currently used universal ACL format matches the definition from
 * illumos acl(5) man page [https://illumos.org/man/5/acl]. This applies
 * to both POSIX draft ACLs and NFSv4 ACLs. See the note on cross-platform
 * synchronization below.
 *
 * ACL is always in the form
 *
 *   acl_entry[,acl_entry]...
 *
 * but there is some helpful post-processing done in props.ml. If the
 * returned string contains newlines then those are replaced with commas
 * in props.ml. If the returned string contains spaces then those are
 * removed in props.ml.
 *
 * Each acl_entry may be suffixed with a colon and userid/groupid.
 *
 * Examples:
 *
 *   POSIX draft ACL
 *
 *     user:tom:rw-,mask:rwx,group:staff:r-x:450
 *
 *   NFSv4 ACL
 *
 *        user:lp:rw------------:------I:allow:1300,
 *         owner@:--x-----------:------I:deny,
 *         owner@:rw-p---A-W-Co-:-------:allow,
 *     user:marks:r-------------:------I:deny:1270,
 *         group@:r-------------:-------:allow,
 *      everyone@:r-----a-R-c--s:-------:allow
 *
 *     (note that the example is folded, but it should actually be
 *     returned as one string line without newlines)
 *
 *
 * ===========
 * On cross-platform synchronization
 *
 * Currently there is no canonical ACL representation created specifically
 * for Unison. Existing platform APIs are used as much as possible, without
 * custom formatting and parsing.
 * A specific Unison ACL format could be truly common across platforms.
 *
 * If extended ACL synchronization capability is desired in the future then
 * it is only required to change the output of unison_acl_to_text() and the
 * input parsing in unison_acl_from_text().
 * The Unison archive format will not have to be changed as long as the
 * entire ACL and any eventual metadata is encoded within one string.
 * It is neither necessary to change the ACL code in props.ml.
 *
 * The issues of such cross-platform (e.g. between Windows and Unix-like)
 * synchronization lie not in the representation format, though. It is easy
 * enough to interpret the permission sets of NFSv4 and Windows ACLs in a
 * similar, equivalent way. It can be much more difficult to interpret the
 * subjects (users and groups) in a meaningful way. Purely for
 * synchronization, this can still work on some platforms, e.g. Solaris,
 * which allow the use of SIDs in ACL definition.
 */

#define UNSN_ACL_NOT_SUPPORTED "-1"

#if defined(sun) || defined(__sun)  /* Solarish, all illumos-based OS,   */
#define __Solaris__                 /* OpenIndiana, OmniOS, SmartOS, ... */
#endif

/* Primitive check only, without explicitly checking for
 * POSIX or NFSv4. NFSv4-style ACLs are expected
 * but POSIX draft ACLs may work to some extent. */
#undef UNSN_HAS_FS_ACL
#if defined(__Solaris__) || defined(__FreeBSD__) || defined(__APPLE__)
#define UNSN_HAS_FS_ACL
#endif

#if defined(__NetBSD__)
#include <unistd.h>
#if defined(_PC_ACL_NFS4)
#define UNSN_HAS_FS_ACL
#endif
#endif

#if defined(__Solaris__)
#include <fcntl.h>
#include <sys/stat.h>
#endif

#if defined(__Solaris__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
#include <sys/types.h>
#include <sys/acl.h>
#endif

#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
#include <unistd.h>
#include <string.h>
#endif

#if defined(__APPLE__)
#include <errno.h>
#endif

#define CAML_NAME_SPACE
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>


#ifndef UNSN_HAS_FS_ACL

CAMLprim value unison_acl_from_text(value path, value acl)
{
  CAMLparam2(path, acl);
  CAMLreturn(Val_unit);
}

CAMLprim value unison_acl_to_text(value path)
{
  CAMLparam1(path);
  CAMLreturn(caml_copy_string(UNSN_ACL_NOT_SUPPORTED));
}

#else /* UNSN_HAS_FS_ACL */


#if defined(__APPLE__)
#define UNSN_ACL_T acl_t
#else
#define UNSN_ACL_T acl_t *
#endif

#if defined(__FreeBSD__) || defined(__NetBSD__)
#define UNSN_ACL_TO_TEXT(aclp)    acl_to_text_np(aclp, NULL, ACL_TEXT_APPEND_ID)
#elif defined(__APPLE__)
#define UNSN_ACL_TO_TEXT(aclp)    acl_to_text(aclp, NULL)
#endif


#if defined(__FreeBSD__) || defined(__NetBSD__)
static acl_type_t unsn_path_acl_type(const char *path)
{
  if (lpathconf(path, _PC_ACL_NFS4) > 0) { /* NFSv4 ACL supported */
    return ACL_TYPE_NFS4;
  } else if (lpathconf(path, _PC_ACL_EXTENDED) > 0) { /* POSIX draft ACL */
    return ACL_TYPE_ACCESS; /* It is not possible to get or set
                               default and access ACL at the same time,
                               so fall back to access ACL only. */
  } else { /* ACLs not supported */
    return -1;
  }
}
#elif defined(__APPLE__)
static acl_type_t unsn_path_acl_type(const char *path)
{
  return ACL_TYPE_EXTENDED;
}
#endif


#if defined(__Solaris__)
static void unsn_remove_acl(const char *path)
{
  static struct stat st;

  if (stat(path, &st) != 0) {
    caml_failwith("Can't access file to remove ACL");
  }

  if (acl_strip(path, st.st_uid, st.st_gid, st.st_mode) != 0) {
    caml_failwith("Error removing ACL");
  }
}
#elif defined(__FreeBSD__) || defined(__NetBSD__)
static void unsn_remove_acl(const char *path)
{
  /* FreeBSD has a acl_strip_np() function, but it would be
   * much too complicated in this code. */
  /* Don't even bother checking for target ACL type, just
   * try to remove all and ignore errors. */
  acl_delete_link_np(path, ACL_TYPE_DEFAULT);
  acl_delete_link_np(path, ACL_TYPE_ACCESS);
  acl_delete_link_np(path, ACL_TYPE_NFS4);
}
#elif defined(__APPLE__)
static void unsn_remove_acl(const char *path)
{
  acl_set_link_np(path, unsn_path_acl_type(path), acl_from_text("!#acl 1"));
}
#else
static void unsn_remove_acl(const char *path)
{
}
#endif


#if defined(__FreeBSD__) || defined(__NetBSD__)
static void postprocess_acl(char **s)
{
  char *p, *start;
  char *buf;
  int perms = 0, comment = 0, offs = 0;

  buf = malloc(strlen(*s) + 1);
  if (buf == NULL) {
    return;
  }

  for (p = *s; *p; p++) {
    switch (*p) {
      case ',' :
          perms = 0;
          break;
      case '#' :
          /* FreeBSD acl_to_text embeds the #effective permissions,
           * which are actually not part of the ACL. */
          comment = 1;
          break;
      case '@' :
      case ':' :
          if (!comment) {
            perms++;
          }
          break;
      case 'D' :
          /* Swap the position of d and D permissions.
           * Synchronization works even without swapping, but the different
           * ordering will show up as constant synchronization difference. */
          if (perms == 2) {
            if (buf[offs - 1] != 'd' && *(p + 1) == 'd') {
              *p = 'd';
              *(p + 1) = 'D';
            } else if (buf[offs - 1] != 'd' && *(p + 1) == '-') {
              *p = '-';
              *(p + 1) = 'D';
            }
            perms = 0; /* prevent further swapping */
          }
          break;
      case 'd' :
          if (perms == 2) {
            if (buf[offs - 1] == '-' && *(p + 1) != 'D') {
              buf[offs - 1] = 'd';
              *p = '-';
            } else if (buf[offs - 1] != 'd' && *(p + 1) == '-') {
              *p = '-';
              *(p + 1) = 'D';
            }
            perms = 0; /* prevent further swapping */
          }
          break;
      case '\n' :
          /* Replace newlines with commas...
           * ... except if it's the last one. */
          if (*(p + 1) != '\0') {
            *p = ',';
          } else {
            *p = ' ';
          }
          perms = 0;
          comment = 0;
          break;
    }

    /* Remove all whitespace and comments. */
    if (*p != ' ' && *p != '\t' && !comment) {
      buf[offs++] = *p;
    }
  }
  buf[offs] = '\0';

  free(*s);
  *s = buf;
}
#elif defined(__APPLE__)
static void postprocess_acl(char **s)
{
  /* Remove trailing newline */
  size_t last = strlen(*s) - 1;
  if (last >= 0 && (*s)[last] == '\n') {
    (*s)[last] = '\0';
  }
}
#endif


/************************************
 *         Set ACL from text
 ************************************/
CAMLprim void unison_acl_from_text(value path, value acl)
{
  CAMLparam2(path, acl);
  const char *acl_text = String_val(acl);
  const char *name = String_val(path);
  UNSN_ACL_T aclp = NULL;
  int error;

#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
  acl_type_t type = unsn_path_acl_type(name);
  if (type == -1) {
    caml_failwith("ACL not supported");
  }
#endif

#if defined(__Solaris__)
  /* Safety check so we don't follow links */
  static struct stat st;

  if (stat(name, &st) != 0) {
    caml_failwith("Can't access file to set ACL");
  }
  if ((st.st_mode & S_IFMT) == S_IFLNK) {
    CAMLreturn0;
  }
#endif

  /* Check if ACL must be removed */
  if (acl_text == NULL || *acl_text == '\0') {
    unsn_remove_acl(name);
    CAMLreturn0;
  }

#if defined(__Solaris__)
  error = acl_fromtext(acl_text, &aclp);

  if (error == 0 && aclp != NULL) {
    error = acl_set(name, aclp);
#elif defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
  aclp = acl_from_text(acl_text);

  if (aclp != NULL) {
    error = acl_set_link_np(name, type, aclp);
#endif
    acl_free(aclp);

    if (error != 0) {
      caml_failwith("Error setting ACL");
    }
  } else {
    caml_failwith("Error converting ACL from text");
  }

  CAMLreturn0;
}


/************************************
 *          Get ACL as text
 ************************************/
CAMLprim value unison_acl_to_text(value path)
{
  CAMLparam1(path);
  CAMLlocal1(result);
  const char *name = String_val(path);
  UNSN_ACL_T aclp = NULL;

#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
  int is_trivial = 0;

  acl_type_t type = unsn_path_acl_type(name);
  if (type == -1) {
    CAMLreturn(caml_copy_string(UNSN_ACL_NOT_SUPPORTED));
  }

  aclp = acl_get_link_np(name, type);

#if defined(__FreeBSD__) || defined(__NetBSD__)
  if (aclp != NULL) {
    acl_is_trivial_np(aclp, &is_trivial); /* Ignore any errors here */
#elif defined(__APPLE__)
  /* ACLs are always enabled on Darwin since version 10 (2009) */
  if (aclp != NULL || errno != EOPNOTSUPP) {
    if (aclp == NULL) is_trivial = 1;
#endif

    if (is_trivial == 0 && aclp != NULL) {
      char *s = UNSN_ACL_TO_TEXT(aclp);
      postprocess_acl(&s);
#endif  /* FreeBSD or NetBSD or Darwin */

#if defined(__Solaris__)
  int error = acl_get(name, ACL_NO_TRIVIAL, &aclp);
  if (error == 0) {
    if (aclp != NULL) {
      char *s = acl_totext(aclp, ACL_APPEND_ID | ACL_COMPACT_FMT | ACL_SID_FMT);
#endif
      acl_free(aclp);

      if (s != NULL) {
        result = caml_copy_string(s);
        free(s);
      } else {
        caml_failwith("Error converting ACL to text");
      }
    } else {
      /* Empty ACL (or only trivial) */
      result = caml_copy_string("");
    }
  } else {
    /* Error getting ACL
     * Ignore this silently as this may signal ACLs not supported, which is ok */
    result = caml_copy_string(UNSN_ACL_NOT_SUPPORTED);
  }

  CAMLreturn(result);
}

#endif /* UNSN_HAS_FS_ACL */
