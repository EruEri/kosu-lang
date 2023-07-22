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
#include <stdlib.h>
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/fail.h"
#include "../core/kosuvm.h"
#include <ffi/ffi.h>

#define CLOS_CAML_LIST_LENGTH "caml_list_length"

static mlsize_t caml_list_length(value l) {
    static const value* closure = NULL;
    if (!closure) closure = caml_named_value(CLOS_CAML_LIST_LENGTH);
    value length = caml_callback(*closure, l);
    return Long_val(length);
     
}

static value val_of_vm(kosuvm_t* vm) {
    value v = caml_alloc(1, Abstract_tag);
    *((kosuvm_t **) Data_abstract_val(v)) = vm;
    return v;
}

static kosuvm_t* vm_of_value(value vm) {
    return *((kosuvm_t **) Data_abstract_val(vm));
}

const char** caml_clibs(value libs, size_t len) {
    const char** c_libs = malloc(sizeof(char*) * (len + 1));
    size_t i = 0;
    if (!c_libs) return NULL;
    value head;
    while ( libs != Val_emptylist )
    {
        head = Field(libs, 0);  
        const char* c_str = String_val(head);
        *(c_libs + i) = c_str;
        libs = Field(libs, 1);
        i = i + 1;
    }
    c_libs[len] = NULL;
    return c_libs;
}

arg_t caml_address(value caml_args) {
    arg_t a;
    switch (Tag_val(caml_args)) {
        case AT_VALUE: {
            value caml_value = Field(caml_args, 0);
            int64_t c_value = Int64_val(caml_value);
            a.tag = AT_VALUE;
            a.offset.o_value = c_value;
            break;
        }
        case AT_REG: {
            a.tag = AT_REG;
            value assoc_record = Field(caml_args, 0);
            uint8_t base_reg = Int64_val(Field(assoc_record, 0));
            a.enc_reg = base_reg;
            value address_offset_value = Field(assoc_record, 1);
            address_offset_tag_t aot_tag = Tag_val(address_offset_value);
            address_offset_t aot = {.aot_tag = aot_tag};

            switch (aot_tag) {
            case AOT_REG: {
                uint8_t o_reg = Int64_val(Field(address_offset_value, 0)); 
                aot.aot_val.aot_enc_reg = o_reg;
                break;
            }
            case AOT_VALUE: {
                int64_t o_value= Int64_val(Field(address_offset_value, 1)); 
                aot.aot_val.value = o_value;
                break;
            }
            default:
                caml_failwith("Unknwon tag for address_offset_tag_t");
            }
            a.offset.o_reg = aot;
            break;
        }
        case AT_PC_REL:{
            a.tag = AT_PC_REL;
            a.offset.o_pcrel = Int_val(Field(caml_args, 0));
            break;
        }
        default:
            caml_failwith("Unknwon tag for caml_address");
    }
    return a;
}

ffi_type caml_ffi_type(value ffi_type) {
    exit(3);
}

CAMLprim value caml_kosuvm_init_bis(value code, value stack_size, value start_index, value libs, value ccentries) {
    CAMLparam5(code, stack_size, start_index, libs, ccentries);
    unsigned long index = Long_val(start_index);
    unsigned long cstack_size = Val_long(stack_size);
    const void * vm_code = String_val(code);
    const size_t libs_length = caml_list_length(libs);
    const size_t ccentries_length = caml_list_length(ccentries); 
    const char** c_clibs = caml_clibs(libs, libs_length);
    if (!c_clibs) {
        caml_failwith("Fail to alloc clibs");
    } 
}

// code : bytes or string
// start_index : int
// string -> int -> int -> unit -> vm
CAMLprim value caml_kosuvm_init(value code, value stack_size, value start_index, value unit) {
    CAMLparam4(code, stack_size, start_index, unit);
    unsigned long index = Long_val(start_index);
    unsigned long cstack_size = Val_long(stack_size);
    const void * vm_code = String_val(code);
    kosuvm_t* vm = kosuvm_init(vm_code, cstack_size, index);
    CAMLreturn(val_of_vm(vm));
}

CAMLprim value caml_kosuvm_run(value vm, value unit) {
    CAMLparam2(vm, unit);
    CAMLlocal1(ret);
    kosuvm_t* c_vm = vm_of_value(vm);
    int status = kosuvm_run(c_vm);
    ret = Val_int(status);
    CAMLreturn(ret);
}

CAMLprim value caml_kosuvm_run_single(value vm, value unit) {
    CAMLparam2(vm, unit);
    CAMLlocal1(ret);
    kosuvm_t* c_vm = vm_of_value(vm);
    int status = kosuvm_run_single(c_vm);
    ret = Val_int(status);
    CAMLreturn(ret);
}

// vm -> unit -> unit
CAMLprim value caml_kosuvm_free(value vm, value unit) {
    CAMLparam2(vm, unit);
    kosuvm_t* cvm = vm_of_value(vm);
    kosuvm_free(cvm); 
    CAMLreturn(Val_unit);
}