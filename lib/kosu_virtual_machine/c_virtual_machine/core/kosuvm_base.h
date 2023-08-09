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

#ifndef __KOSUVM_BASE_H__
#define __KOSUVM_BASE_H__

#include <stdint.h>
#include <stddef.h>
#include "util.h"
#include "ffi.h"

#define HALT_BITS 0x0
#define RET_BITS 0x01
#define SYSCALL_BITS 0x2
#define CALL_BITS 0x3


#define SHIFT_ONLY_MASK 0x3
#define REG_ONLY_MASK 0x1F
#define CC_ONLY_MASK 0xF
#define BR_JMP_MASK 0X3
#define DATA_SIZE_MASK 0x3

#define VM_INSTR_SUCESS 0
#define VM_HALT_EXIT_CODE 1

extern const uint32_t KOSUVM_OPCODE_MASK;
extern const uint32_t KOSUVM_INSTRUCTION_SIZE;
extern const uint32_t KOSUVM_OPCODE_SIZE;
extern const uint32_t KOSUVM_CONDITION_CODE_SIZE;
extern const uint32_t KOSUVM_REGISTER_SIZE;
extern const uint32_t KOSUVM_LD_ST_DATA_SIZE;
extern const uint32_t KOSUVM_WORD_SIZE;

typedef uint64_t reg_t;
typedef uint64_t freg_t;
typedef uint32_t instruction_t;

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
    CMP_CSET, 
    LDR_STR,
    ITOF_FTOI,

} kosuvm_opcode_t;


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
    uint8_t* const memory;
    const uint64_t size;
    reg_t sp;
} kosuvm_stack_t;

typedef enum {
    AOT_REG = 0,
    AOT_VALUE = 1,
} address_offset_tag_t;

typedef struct {
    address_offset_tag_t aot_tag;
    uint8_t aot_base_reg_addr;
    union {
        // encoding of reg
        uint8_t aot_enc_reg;
        int64_t value;
    } aot_val;
} address_offset_t;

typedef enum {
    AT_VALUE = 0,
    AT_ADDR,
    AT_PC_REL
} arg_tag_t;

typedef struct {
    arg_tag_t tag;
    union {
        address_offset_t o_addr_off;
        int64_t o_value;
        int64_t o_pcrel;
    } offset;
} arg_t;

typedef struct {
    const arg_t* p_address;
    size_t p_count;
} args_t;


typedef struct {
    const char* function_name;
    int arity;
    args_t args;
    ffi_type** args_types;
    ffi_type* return_type;
} ccall_entry_t;

typedef struct {
    ccall_entry_t* entries;
    size_t e_count;
} ccall_entries_t;


#define NB_DYNLIB 20

typedef struct {
    void* handlers[NB_DYNLIB];
    size_t count;
} dl_handlers_t;

typedef struct {
    bool_t last_cmp;

    instruction_t const * const code;
    const instruction_t* ip;
    const char** argv;

    kosuvm_stack_t* stack;
    dl_handlers_t dl_handlers;
    ccall_entries_t cc_entries;

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
} kosuvm_t;


address_offset_t addr_offset(address_offset_tag_t is_value, uint8_t base_reg_addr, int64_t value);
arg_t arg_address(address_offset_tag_t is_value, uint8_t base_reg_addr, int64_t value);
arg_t arg_value(int64_t o_value);
arg_t arg_pc_rel(int64_t o_pcrel);


kosuvm_stack_t* kosuvm_stack_create(uint64_t size);
void kosuvm_stack_free(kosuvm_stack_t* stack);
bool_t kosuvm_stack_is_empty(kosuvm_stack_t* stack);
bool_t kosuvm_stack_stack_push(kosuvm_stack_t* stack, uint64_t value);
uint64_t kosuvm_stack_stack_t_pop(kosuvm_stack_t* stack);


#endif