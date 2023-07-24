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


#include <stdio.h>
#include "util.h"
#include "kosuvm_base.h"
#include "kosuvm_util.h"
#include "kosuvm_pp.h"


int show_reg(const char* regname, reg_t reg, bool_t is_float) {
    if (is_float) {
        printf("%s = %f\n", regname, double_of_bits(reg));
    } else {
        printf("%s = %ld\n", regname,  (long int) reg);
    }

    return 0;
}

const char* repr_shift(int value) {
    switch (value & SHIFT_ONLY_MASK) {
    case 0:
        return "shl0";
    case 1:
        return "shl16";
    case 2:
        return "shl32";
    case 3:
        return "shl48";
    }

    // unreachable
    return NULL;
}

const char* repr_data_size(data_size_t ds) {
    ds = ds & DATA_SIZE_MASK;
    switch (ds) {
    case S8:
        return ".8";
    case S16:
        return ".16";
    case S32:
        return ".S32";
    case S64:
        return ".s64";
    default:
        failwith("Unknwon size", ds);
    }
} 

const char* repr_condition_code(condition_code_t cc) {
    cc = cc & CC_ONLY_MASK;
    switch (cc) {
    case ALWAYS:
        return ".al";
    case EQUAL:
        return ".eq";
    case DIFF:
        return ".neq";
    case SUP:
        return ".a";
    case UNSIGNED_SUP:
        return ".ua";
    case SUPEQ:
        return ".aeq";
    case UNSIGNED_SUPEQ:
        return ".uaeq";
    case INF:
        return ".b";
    case UNSIGNED_INF:
        return "ub";
    case INFEQ:
        return ".beq";
    case UNSIGNED_INFEQ:
        return ".ubeq";
    default:
        failwith("Unk", cc); 
    }
    return NULL;
}

const char* repr_register(int value) {
    switch (value & REG_ONLY_MASK) {
    case 0:
        return "r0";
    case 1:
        return "r1";
    case 2:
        return "r2";
    case 3:
        return "r3"; 
    case 4:
        return "r4"; 
    case 5:
        return "r5"; 
    case 6:
        return "r6"; 
    case 7:
        return "r7"; 
    case 8:
        return "r8";
    case 9:
        return "r9";
    case 10:
        return "r10";
    case 11:
        return "r11";
    case 12:
        return "r12";
    case 13:
        return "r13";
    case 14:
        return "r14";
    case 15:
        return "fr0"; 
    case 16:
        return "fr1"; 
    case 17:
        return "fr2"; 
    case 18:
        return "fr3"; 
    case 19:
        return "fr4"; 
    case 20:
        return "fr5"; 
    case 21:
        return "fr6"; 
    case 22:
        return "fr7"; 
    case 23:
        return "fr8";
    case 24:
        return "fr9";
    case 25:
        return "fr10";
    case 26:
        return "ir";
    case 27:
        return "scp";
    case 28:
        return "fp";
    case 29:
        return "rap";
    case 30:
        return "sp";
    default:
        failwith("Wrong register number", 1);
    }
    return 0;
}

int show_status(kosuvm_t* vm) {
    printf("last_cmp = %u\n", vm->last_cmp);
    printf("ip = %p\n", vm->ip - (uint64_t) vm->code);
    printf("fp = %llx\n", vm->fp);
    printf("sp = %llx\n", vm->stack->sp);
    printf("sc = %lu\n", (long int) vm->scp);
    printf("ir = %p\n", (void *) vm->irp);
    printf("ra = %p\n", (void *) vm->rap);
    show_reg("r0", vm->r0, false);
    show_reg("r1", vm->r1, false);
    show_reg("r2", vm->r2, false);
    show_reg("r3", vm->r3, false);
    show_reg("r4", vm->r4, false);
    show_reg("r13", vm->r13, false);
    show_reg("r14", vm->r14, false);

    show_reg("f0", vm->fr0, true);
    show_reg("f1", vm->fr1, true);
    show_reg("f2", vm->fr2, true);
    show_reg("f3", vm->fr3, true);
    show_reg("f4", vm->fr4, true);
    puts("");
    return 0;
}

int pp_halt(const instruction_t i) {
    printf("halt");
    return 0;
}

int pp_ret(const instruction_t i) {
    printf("ret");
    return 0;
}

int pp_syscall(const instruction_t i) {
    printf("syscall");
    return 0;
}

int pp_ccall(const instruction_t i) {
    printf("cc");
    return 0;
}

int pp_0(const instruction_t i) {
    switch ((i >> 25) & 0x3) {
        case HALT: {
            return pp_halt(i);
        }
        case RET_BITS: {
            return pp_ret(i);
        }

        case SYSCALL_BITS: {
            return pp_syscall(i);
        }

        case CALL_BITS: {
            return pp_ccall(i);
        }
    }
    return 0;
}


/// print any mv
/// 0 -> mv
/// 1 -> mvng
/// 2 -> mvnt
int pp_mv_(const instruction_t i, int which) {
    const char* ni = which == 0 ? "mv" : (which == 1) ? "mvng" : (which == 2) ? "mvnt" : "unknown";
    const char* dst = repr_register(i >> 22);
    bool_t is_register = (i >> 21) & 1;
    if (is_register) {
        const char* src = repr_register(i >> 16);
        printf("%s %s %s", ni, dst, src);
    } else {
        int64_t value = sext21(i);
        printf("%s %s %lld", ni, dst,  (long long int) value);
    }
    return 0;
}

int pp_mv(const instruction_t i) {
    return pp_mv_(i, 0);
}

int pp_mvng(const instruction_t i) {
    return pp_mv_(i, 1);
}

int pp_mvnt(const instruction_t i) {
    return pp_mv_(i, 2);
}

int pp_mva(const instruction_t i) {
    const char* op = "mva";
    const char* dst = repr_register(i >> 22);
    const char* shift = repr_register(i >> 20);
    bool_t is_reg = is_set(i, mask_bit(19));
    if (is_reg) {
        const char* src = repr_register(i >> 14);
        printf("%s %s, %s, %s", op, dst, src, shift);
    } else {
        int64_t value = sext18(i);
        printf("%s %s, %lld, %s", op, dst, value, shift);
    } 
    return 0;
}



int pp_br_jmp(const instruction_t i) {
    bool_t is_branch_link = is_set(i, mask_bit(26));
    bool_t is_register = is_set(i, mask_bit(25));
    const char* op = is_branch_link ? "br" : "jump";
    if (is_register) {
        const char* src = repr_register(i >> 20);
        printf("%s *%s", op, src);
    } else {
        int64_t value = sext25(i);
        printf("%s %lld", op, value );
    }
    return 0;
}

int pp_lea(const instruction_t i) {
    const char* repr_lea = "lea";
    const char* dst = repr_register(i >> 22);
    bool_t is_address = is_set(i, mask_bit(21));
    if (is_address) {
        const char* addr_base = repr_register(i >> 16);
        bool_t is_value = is_set(i, mask_bit(15));
        if (is_value) {
            const char* addr_off = repr_register(i >> 10);
            printf("%s %s *(%s, %s)", repr_lea, dst, addr_base, addr_off);
        } else {
            int64_t offset = sext15(i);
            printf("%s %s *(%s, %lld)", repr_lea, dst, addr_base, offset);
        }
    } else {
        int64_t value = sext21(i);
        printf("%s %s *(ip, %lld)", repr_lea, dst, value);
    }
    return 0;
}


int pp_arithm(const char* op, const instruction_t i) {
    const char* dst = repr_register(i >> 22);
    const char* src = repr_register(i >> 17);
    bool_t is_register = is_set(i, mask_bit(16));
    if (is_register) {
        const char* src2 = repr_register(i >> 11);
        printf("%s %s %s %s", op, dst, src, src2);
    } else {
        int64_t value = sext16(i);
        printf("%s %s %s %lld", op, dst, src, value);
    }
    return 0;
}

#define pp_op(name) \
    int pp_##name(const instruction_t i) {\
        return pp_arithm(#name, i); \
    \
    }

pp_op(add)
pp_op(sub)
pp_op(mult)
pp_op(and)
pp_op(or)
pp_op(xor)
pp_op(lsl)
pp_op(asr)
pp_op(lsr)

int pp_div(const instruction_t i) {
    printf("div");
    return -1;
}

int pp_mod(const instruction_t i) {
    printf("mod");
    return -1;
}

int pp_cmp_cset(const instruction_t i){
    const char* cc = repr_condition_code(i >> 23);
    bool_t is_cset = is_set(i, mask_bit(22));
    const char* op = is_cset ? "cset" : "cmp";
    const char* lhs = repr_register(i >> 17);
    const char* rhs = repr_register(i >> 12);
    if (is_cset) {
        const char* update = is_set(i, mask_bit(6)) ? "u" : "";
        const char* dst = repr_register(i >> 7);
        printf("%s%s%s %s, %s, %s", op, update, cc, dst, lhs, rhs);
    } else {
        printf("%s%s %s, %s", op, cc, lhs, rhs);
    }
    return 0;
}

int pp_ldr_str(const instruction_t i) {
    const char* op = is_set(i, mask_bit(26)) ? "str" : "ldr";
    const char* ds = repr_data_size(i >> 24);
    const char* src = repr_register(i >> 19);
    const char* base = repr_register(i >> 14);
    bool_t is_offset_reg = is_set(i, mask_bit(13));
    if (is_offset_reg) {
        const char* offset = repr_register(i >> 12);
        printf("%s%s %s, *(%s, %s)", op, ds, src, base, offset);
    } else {
        int64_t offset = sext13(i);
        printf("%s%s %s, *(%s, %lld)", op, ds, src, base, offset);
    }
    return 0;
}

int pp_itof_ftoi(const instruction_t i) {
    const char* unsign = is_set(i, mask_bit(25)) ? "" : "u";
    const char* op = is_set(i, mask_bit(26)) ? "ftoi" : "itof";
    const char* ds = repr_data_size(i >> 23);
    const char* dst = repr_register(i >> 18);
    const char* src = repr_register(i >> 13);
    printf("%s%s%s %s, %s", unsign, op, ds, dst, src);
    return 0;
}

int pp_instruction(const instruction_t i) {
    kosuvm_opcode_t ist = opcode_value(i);
    switch (ist) {
    case HALT:
        return pp_0(i);
    case MVNOT:
        return pp_mvnt(i);
    case MVNEG:
        return pp_mvng(i);
    case MOV:
        return pp_mv(i);
    case MVA:
        return pp_mva(i);
    case BR_JUMP:
        return pp_br_jmp(i);
    case LEA:
        return pp_lea(i);
    case ADD:
        return pp_add(i);
    case SUB:
        return pp_sub(i);
    case MULT:
        return pp_mult(i);
    case DIV:
        return pp_div(i);
    case MOD:
        return pp_mod(i);
    case AND:
        return pp_and(i);
    case OR:
        return pp_or(i);
    case XOR:
        return pp_xor(i);
    case LSL:
        return pp_lsl(i);
    case LSR:
        return pp_lsr(i);
    case ASR:
        return pp_asr(i);
    case CMP_CSET:
        return pp_cmp_cset(i);
    case LDR_STR:
        return pp_ldr_str(i);
    case ITOF_FTOI:
        return pp_itof_ftoi(i);
    default:
        fprintf(stderr, "Unknown op: %u\n", ist);
        failwith("", 2);
    }
    return 0;
}