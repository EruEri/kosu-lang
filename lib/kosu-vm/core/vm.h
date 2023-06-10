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

#include "vm_base.h"
#include "stack.h"
#include "util.h"
#include <stdint.h>

typedef enum {
    HALT = 0,
    MVNOT = 1,
    MVNEG = 2,
    MOV = 3,
    MVA = 4,
    BR_JUMP = 5,
    LEA = 6,
    ADD,
    SUB,
    MULT,
    DIV,
    MOD,
    AND,
    OR,
    XOR,
    LSL,
    LSR,
    ASR,
    CMP, 
    CSET,
    LDR,
    STR
} vm_opcode_t;


typedef enum {
    ALWAYS,
    EQUAL,
    DIFF,
    SUP,
    UNSIGNED_SUP,
    SUPEQ,
    UNSIGNED_SUPEQ,
    INF,
    UNSIGNED_INF,
    INFEQ,
    UNSIGNED_INFEQ
} condition_code_t;

typedef enum {
    S8,
    S16,
    S32,
    S64
} data_size_t;

typedef enum {
    SUCCESS,
    STOP,
    LDR_ERROR,
} vm_status_kind_t;

typedef struct vm_return_t {
    int status; // 0 == success, -1 erreur
    struct {
        uint32_t op;
        const char*const message;
    } reason;
} vm_return_t;

typedef struct {
    instruction_t const * const code;
    bool_t last_cmp;
    const instruction_t* ip;
    vm_stack_t* stack;


    // Register parameters
    reg_t r0;
    reg_t r1;
    reg_t r2;
    reg_t r3;
    reg_t r4;
    reg_t r5;
    reg_t r6;
    reg_t r7;
    reg_t r8;
    reg_t r9;
    reg_t r10;
    reg_t r11;
    reg_t r12;
    reg_t r13;
    reg_t r14;
    // Register parameters float
    freg_t fr0;
    freg_t fr1;
    freg_t fr2;
    freg_t fr3;
    freg_t fr4;
    freg_t fr5;
    freg_t fr6;
    freg_t fr7;
    freg_t fr8;
    freg_t fr9;
    freg_t fr10;

    // Indirect return register
    reg_t irp;
    // Syscall code register
    reg_t scp;
    // Frame pointer register
    reg_t fp;
    // Return adress register
    reg_t rap;
} vm_t;


vm_t* vm_init(instruction_t const * const code, uint64_t stack_size, uint64_t offset); 
int show_status(vm_t* vm);
int vm_run(vm_t* vm);
void free_vm(vm_t* vm);
#endif