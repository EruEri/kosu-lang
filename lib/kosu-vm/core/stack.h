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

#ifndef STACK_H
#define STACK_H

#include <stdint.h>
#include "util.h"
#include "vm_base.h"

typedef struct {
    uint8_t* const memory;
    const uint64_t size;
    reg_t sp;
} vm_stack_t;

vm_stack_t* stack_create(uint64_t size);
void free_stack(vm_stack_t* stack);
bool_t is_empty(vm_stack_t* stack);
bool_t push(vm_stack_t* stack, uint64_t value);
uint64_t pop(vm_stack_t* stack);

#endif