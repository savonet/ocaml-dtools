/*
 * Copyright 2013 Savonet team
 *
 * This file is part of Ocaml-dtools.
 *
 * Ocaml-dtools is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-dtools is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-dtools; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <caml/memory.h>

#include <sys/mman.h>

#ifdef USE_MLOCK

CAMLprim value ocaml_dtools_mlockall() {
  CAMLparam0();

  caml_enter_blocking_section();
  int ret = mlockall(MCL_CURRENT | MCL_FUTURE);
  caml_leave_blocking_section();

  if (ret == -1)
    uerror("mlockall", Nothing);

  CAMLreturn(Val_unit);  
}

#else

CAMLprim value ocaml_dtools_mlockall() {
  CAMLparam0();
  uerror("mlockall", Nothing);
  CAMLreturn(Val_unit);
}

#endif
