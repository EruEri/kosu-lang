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



#include <string.h>
#define CAML_NAME_SPACE

#include <stdio.h>
#include <stdlib.h>
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/fail.h"
#include "../core/kosuvm.h"
#include "ffi.h"

#define CLOS_CAML_LIST_LENGTH "c_caml_list_length"

#define DEBUG \
    puts("Hello world"); \
    fflush(stdout);


typedef struct {
    kosuvm_t* vm;
    char** argv;
    ccall_entries_t entries;
} caml_kosuvm_t;

ffi_type* caml_ffi_type(value caml_ffi);
ffi_type** caml_ffi_types_list(value caml_ffi_list);

void free_caml_ffi(ffi_type* ffi);
void free_caml_ffi_array(ffi_type** ffi);

static mlsize_t caml_list_length(value l) {
    static const value* closure = NULL;
    if (!closure) closure = caml_named_value(CLOS_CAML_LIST_LENGTH);
    value length = caml_callback(*closure, l);
    return Long_val(length);
     
}

static value val_of_camlvm(caml_kosuvm_t vm) {
    size_t alloc_size = (sizeof(caml_kosuvm_t) / sizeof(value) ) + 1;
    value v = caml_alloc(alloc_size, Abstract_tag);
    *((caml_kosuvm_t *) Data_abstract_val(v)) = vm;
    return v;
}

static caml_kosuvm_t vm_of_value(value vm) {
    return *((caml_kosuvm_t *) Data_abstract_val(vm));
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

ffi_type** caml_ffi_types_list(value caml_ffi_list) {
     size_t list_len = caml_list_length(caml_ffi_list);
        ffi_type** struture_elt = malloc(sizeof(ffi_type*) * (list_len + 1));
        if (!struture_elt)
            return NULL;

        size_t index = 0;
        while (caml_ffi_list != Val_emptylist) {
            value head = Field(caml_ffi_list, 0);
            *(struture_elt + index) = caml_ffi_type(head);
            caml_ffi_list = Field(caml_ffi_list, 1);
            index += 1;
        }

        *(struture_elt + list_len) = NULL;
        return struture_elt;
}

ffi_type* caml_ffi_type(value caml_ffi) {
    if (Is_block(caml_ffi)) {
        ffi_type* struture = malloc(sizeof(ffi_type));
        if (!struture) return NULL;
        struture->type = FFI_TYPE_STRUCT;
        value caml_ffi_list = Field(caml_ffi, 0);
        ffi_type** struture_elt = caml_ffi_types_list(caml_ffi_list);
        if (!struture_elt) {
            free(struture);
            return NULL;
        }
        struture->elements = struture_elt;
        return struture;
    } else {
        switch (Int_val(caml_ffi)) {
            case 0:
                return &ffi_type_sint8;
            case 1:
                return &ffi_type_uint8;
            case 2:
                return &ffi_type_sint16;
            case 3:
                return &ffi_type_uint16;
            case 4:
                return &ffi_type_sint32;
            case 5:
                return &ffi_type_uint32;
            case 6:
                return &ffi_type_sint64;
            case 7:
                return &ffi_type_uint64;
            case 8:
                return &ffi_type_float;
            case 9:
                return &ffi_type_double;
            case 10:
                return &ffi_type_pointer;
            default:
                return NULL;
        }
    }
}



arg_t caml_arg(value caml_args) {
    // CAMLparam1(caml_args);
    // CAMLlocal2(caml_value, assoc_record);
    switch (Tag_val(caml_args)) {
        case AT_VALUE: {
            value caml_value = Field(caml_args, 0);
            int64_t c_value = Int64_val(caml_value);
            return arg_value(c_value);
        }
        case AT_PC_REL:{
            int64_t c_pcrel = Long_val(Field(caml_args, 0));
            return arg_pc_rel(c_pcrel);
        }
        case AT_ADDR: {
            value assoc_record = caml_args;
            uint8_t base_reg = Int64_val(Field(assoc_record, 0));
            value address_offset_value = Field(assoc_record, 1);
            address_offset_tag_t aot_tag = Tag_val(address_offset_value);
            int64_t value_or_reg;
            if (aot_tag == AOT_REG) {
                uint8_t o_reg = Int64_val(Field(address_offset_value, 0)); 
                value_or_reg = o_reg;
            } else {
                int64_t o_value = Int64_val(Field(address_offset_value, 0));
                value_or_reg = o_value;
            }
            return arg_address(aot_tag, base_reg, value_or_reg);
        }
        default:
            caml_failwith("Unknwon tag for caml_address");
    }
}

args_t caml_args(value caml_args) {

    size_t len = caml_list_length(caml_args);
    arg_t* args = malloc(sizeof(arg_t) * len);
    if (!args) {
        args_t c_args = {.p_count = 0, .p_address = NULL};
        return c_args;
    }
    size_t index = 0;
    while (caml_args != Val_emptylist) {
        value head = Field(caml_args, 0);
        *(args + index) = caml_arg(head);
        caml_args = Field(caml_args, 1);
        index += 1;
    }
    args_t c_args = {.p_address = args, .p_count = len};
    return c_args;
}

ccall_entry_t caml_ccall_entry(value caml_entry) {
    // CAMLparam1(caml_entry);
    const char* function_name = String_val(Field(caml_entry, 0));
    int64_t arity = Int64_val(Field(caml_entry, 1));
    args_t args = caml_args(Field(caml_entry, 2));
    ffi_type** args_types = caml_ffi_types_list(Field(caml_entry, 3));
    ffi_type* return_type = caml_ffi_type(Field(caml_entry, 4));
    ccall_entry_t entry = {
        .function_name = function_name, 
        .arity = arity,
        .args = args,
        .args_types = args_types,
        .return_type = return_type
    };
    return entry;
}

ccall_entries_t caml_ccall_entries(value caml_args) {

    size_t len = caml_list_length(caml_args);    
    ccall_entry_t* entries = malloc(sizeof(ccall_entry_t) * len);
    if (!entries) {
        ccall_entries_t c_args = {.e_count = 0, .entries = NULL};
        return c_args;
    }
    size_t index = 0;
    while (caml_args != Val_emptylist) {
        value head = Field(caml_args, 0);
        *(entries + index) = caml_ccall_entry(head);

        caml_args = Field(caml_args, 1);
        index += 1;
    }
    ccall_entries_t c_entries = {.entries = entries, .e_count = len};
    return c_entries;
}

const char** caml_argv_array(int argc, value argv) {
    size_t count = argc + 1; // TO STORE NULL
    char** cargv = calloc(count, sizeof(const char*));
    if (!argv) return NULL;
    for (size_t i = 0; i < argc; i += 1 ) {
        const char* caml_str = String_val(Field(argv, i));
        const size_t c_strlen = strlen(caml_str) + 1;
        const char* c_str = calloc(c_strlen, sizeof(char));
        if (!c_str) goto error;
        memcpy((void *) c_str, caml_str, c_strlen);
        cargv[i] = (char*) c_str;
    }

    return (const char**) cargv;

    error: {
        for (size_t i = 0; i < argc; i += 1 ) {
            free( (void*) cargv[i] );
        }
        free( cargv );
    }
    return NULL;
}

void free_caml_argv(char** argv) {
    char** base = argv;
    char* current;
    do {
        current = *argv;
        free(current);
        argv += 1;
    } while (current);
    free(base);
}

void free_caml_ffi_array(ffi_type** base) {
    ffi_type** c_base = base;
    ffi_type* current = NULL;
    do {
        current = *base;
        if (current == NULL) break;
        free_caml_ffi(current);
        base += 1;
    } while (current);

    free(c_base);
}

void free_caml_args(args_t e) {
    free( (void *) e.p_address);
}

void free_caml_ffi(ffi_type* ffi) {
    if (!ffi || ffi->type != FFI_TYPE_STRUCT) { return; }
    free_caml_ffi_array(ffi->elements);
    ffi->elements = NULL;
    free(ffi);
}

void free_ccentry(ccall_entries_t entries) {
    for (size_t i = 0; i < entries.e_count; i += 1) {
        ccall_entry_t e = entries.entries[i];
        free_caml_ffi(e.return_type);
        free_caml_ffi_array(e.args_types);
        free_caml_args(e.args);
    }

    free(entries.entries);
}

CAMLprim value caml_kosuvm_init(value argc, value argv, value code, value stack_size, value start_index, value libs, value ccentries) {
    CAMLparam5(argc, argv, code, stack_size, start_index);
    CAMLxparam2(libs, ccentries);

    ccall_entries_t c_entries = caml_ccall_entries(ccentries);

    int cargc = Int_val(argc);
    const char ** cargv = caml_argv_array(cargc, argv);
    unsigned long index = Long_val(start_index);
    unsigned long cstack_size = Val_long(stack_size);
    const void * vm_code = String_val(code);
    kosuvm_t* vm = NULL;

    const size_t libs_length = caml_list_length(libs);
    const char** c_clibs = caml_clibs(libs, libs_length);
    if (!c_clibs || !cargv || !c_entries.entries) {
        goto error;
    }

    vm = kosuvm_init(cargc, cargv, vm_code, cstack_size, index, c_entries, c_clibs);
    if (!vm) goto error;
    caml_kosuvm_t cvm = {.entries = c_entries, .argv = (char**) cargv, .vm = vm};

    CAMLreturn(val_of_camlvm(cvm));

    error:
        free(cargv);
        free(c_clibs);
        free(c_entries.entries);
        free(vm);
        caml_failwith("caml_kosuvm_init_bytecode: an alloc fail");
}

CAMLprim value caml_kosuvm_init_bytecode(value* values, int argn) {
    return caml_kosuvm_init(values[0], values[1], values[2], 
        values[3], values[4], values[5], values[6]
        );
}

CAMLprim value caml_kosuvm_run(value vm, value unit) {
    CAMLparam2(vm, unit);
    CAMLlocal1(ret);
    caml_kosuvm_t c_vm = vm_of_value(vm);
    int status = kosuvm_run(c_vm.vm);
    ret = Val_int(status);
    CAMLreturn(ret);
}

CAMLprim value caml_kosuvm_run_single(value vm, value unit) {
    CAMLparam2(vm, unit);
    CAMLlocal1(ret);
    caml_kosuvm_t c_vm = vm_of_value(vm);
    int status = kosuvm_run_single(c_vm.vm);
    ret = Val_int(status);
    CAMLreturn(ret);
}

// vm -> unit -> unit
CAMLprim value caml_kosuvm_free(value vm, value unit) {
    CAMLparam2(vm, unit);
    caml_kosuvm_t cvm = vm_of_value(vm);
    free_caml_argv(cvm.argv);
    free_ccentry(cvm.entries);
    kosuvm_free(cvm.vm); 
    CAMLreturn(Val_unit);
}