/* Unison file synchronizer: src/hash_xxhash.c */
/* Copyright 2023, Tõivo Leedjärv

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

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/version.h>
#define CAML_INTERNALS
#include <caml/io.h>
#include <string.h>

#define XXH_INLINE_ALL
#include "xxhash_impl.h"

#if OCAML_VERSION < 50100
#define caml_channel_lock Lock
#define caml_channel_unlock Unlock
#endif

static const char unsn_xxh3_64_errmsg[] =
  "Internal error in hash algorithm (XXH3_64)";

void unsn_xxh_err(struct channel *chan, XXH3_state_t* state, const char *errmsg)
{
  XXH3_freeState(state);
  caml_channel_unlock(chan);
  caml_raise_sys_error(caml_copy_string(errmsg));
}

value unsn_xxh3_64_channel(struct channel *chan, intnat toread)
{
  CAMLparam0();
  XXH3_state_t* state;
  value res;
  intnat read;
  char buffer[16384];

  caml_channel_lock(chan);
  state = XXH3_createState();
  if (state == NULL) {
    caml_raise_out_of_memory();
  }
  if (XXH3_64bits_reset(state) == XXH_ERROR) {
    unsn_xxh_err(chan, state, unsn_xxh3_64_errmsg);
  }

  if (toread < 0) {
    while (1) {
      read = caml_getblock(chan, buffer, sizeof(buffer));
      if (read == 0) break;
      if (XXH3_64bits_update(state, buffer, read) == XXH_ERROR) {
        unsn_xxh_err(chan, state, unsn_xxh3_64_errmsg);
      }
    }
  } else {
    while (toread > 0) {
      read = caml_getblock(chan, buffer,
                           toread > sizeof(buffer) ? sizeof(buffer) : toread);
      if (read == 0) caml_raise_end_of_file();
      if (XXH3_64bits_update(state, buffer, read) == XXH_ERROR) {
        unsn_xxh_err(chan, state, unsn_xxh3_64_errmsg);
      }
      toread -= read;
    }
  }

  res = caml_alloc_string(sizeof(XXH64_canonical_t));
  XXH64_canonicalFromHash((XXH64_canonical_t *)&Byte_u(res, 0),
                          XXH3_64bits_digest(state));
  XXH3_freeState(state);
  caml_channel_unlock(chan);
  CAMLreturn (res);
}

CAMLprim value unsn_xxh3_64_chan(value vchan, value len)
{
   CAMLparam2(vchan, len);
   CAMLreturn(unsn_xxh3_64_channel(Channel(vchan), Long_val(len)));
}
