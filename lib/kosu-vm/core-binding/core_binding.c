////////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                            //
// This file is part of Kosu                                                                  //
// Copyright (C) 2023 Yves Ndiaye                                                             //
//                                                                                            //
// Kosu is free software: you can redistribute it and/or modify it under the terms            //
// of the GNU General Public License as published by the Free Software Foundation,            //
// either version 3 of the License, or (at your option) any later version.                    //
//                                                                                            //
// Kosu is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;          //
// without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           //
// PURPOSE.  See the GNU General Public License for more details.                             //
// You should have received a copy of the GNU General Public License along with Kosu  .       //
// If not, see <http://www.gnu.org/licenses/>.                                                //
//                                                                                            //
////////////////////////////////////////////////////////////////////////////////////////////////

#define CAML_NAME_SPACE

#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "../core/vm.h"


static value val_of_vm(vm_t* vm) {
    value v = caml_alloc(1, Abstract_tag);
    *((vm_t **) Data_abstract_val(v)) = vm;
    return v;
}

static vm_t* vm_of_value(value vm) {
    return *((vm_t **) Data_abstract_val(vm));
}

// code : bytes or string
// start_index : int
// string -> int -> int -> unit -> vm
CAMLprim value caml_vm_init(value code, value stack_size, value start_index, value unit) {
    CAMLparam4(code, stack_size, start_index, unit);
    unsigned long index = Long_val(start_index);
    unsigned long cstack_size = Val_long(stack_size);
    const void * vm_code = String_val(code);
    vm_t* vm = vm_init(vm_code, cstack_size, index);
    CAMLreturn(val_of_vm(vm));
}


// vm -> unit -> unit
CAMLprim value caml_vm_free(value vm, value unit) {
    CAMLparam2(vm, unit);
    vm_t* cvm = vm_of_value(vm);
    free_vm(cvm); 
    CAMLreturn(Val_unit);
}