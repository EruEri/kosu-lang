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

#include "kosuvm_base.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

const uint32_t KOSUVM_OPCODE_MASK = 0b11111000000000000000000000000000;
const uint32_t KOSUVM_INSTRUCTION_SIZE = 32;
const uint32_t KOSUVM_OPCODE_SIZE = 5;
const uint32_t KOSUVM_CONDITION_CODE_SIZE = 4;
const uint32_t KOSUVM_REGISTER_SIZE = 5;
const uint32_t KOSUVM_LD_ST_DATA_SIZE = 2;
const uint32_t KOSUVM_WORD_SIZE = 8;

kosuvm_stack_t* kosuvm_stack_create(uint64_t size) {
    kosuvm_stack_t* stack_ptr = malloc(sizeof(kosuvm_stack_t));
    if (!stack_ptr) failwith("Stack alloc failed", 1);

    uint64_t alligned_size = align8(size);
    uint64_t alloc_size = alligned_size * sizeof(uint64_t);

    uint8_t* memory = malloc(alloc_size);
    if (!memory) failwith("Malloc failed", 1);
    kosuvm_stack_t stack = {.memory = memory, .size = alligned_size, .sp = (reg_t) memory + alloc_size};
    memcpy(stack_ptr, &stack, sizeof(kosuvm_stack_t));
    return stack_ptr;
}

void kosuvm_stack_free(kosuvm_stack_t* stack) {
    free(stack->memory);
    free(stack);
}

bool_t kosuvm_stack_is_empty(kosuvm_stack_t* stack) {
    return stack->sp == 0;
}

bool_t kosuvm_stack_is_full(kosuvm_stack_t* stack) {
    return stack->sp >= stack->size;
}

bool_t kosuvm_stack_push(kosuvm_stack_t* stack, uint64_t value){
    if (stack->sp == stack->size) return false;
    stack->memory[stack->sp++] = value;
    return true;
}

uint64_t kosuvm_stack_pop(kosuvm_stack_t* stack){
    if (stack->sp == 0) failwith("Empty stack", 1);
    return stack->memory[stack->sp--];
}

void kosuvm_stack_set_n(kosuvm_stack_t* stack, uint64_t value, uint64_t index) {
    if (index >= stack->sp || index <= 0 ) failwith("Wrong index stack set", 1);
    stack->memory[index] = value;
    return;
}

bool_t kosuvm_stack_alloc_n(kosuvm_stack_t* stack, uint64_t size) {
    if (stack->sp + size > stack->size) {
        return false;
    } else {
        stack->sp = stack->sp + size;
        return true;
    }
}


