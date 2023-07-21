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

#ifndef VM_H
#define VM_H

#include "kosuvm_base.h"
#include "util.h"
#include <stdint.h>
#include <stddef.h>
#include "ffi/ffi.h"


typedef struct {
    reg_t base_reg;
    int reg;
    union {
        reg_t o_reg;
        int o_value;
    } offset;
} address_t;

typedef struct {
    const address_t* p_address;
    const size_t p_count;
} addresses_t;


typedef struct {
    const char* function_name;
    int arity;
    size_t dynlib_entry;
    addresses_t addresses;
    ffi_type** args;
    ffi_type* return_type;
} ccall_entry_t;



kosuvm_t* kosuvm_init(instruction_t const * const code, uint64_t stack_size, uint64_t offset); 
int kosuvm_run(kosuvm_t* vm);
int kosuvm_run_single(kosuvm_t* vm);
void kosuvm_free(kosuvm_t* vm);
#endif