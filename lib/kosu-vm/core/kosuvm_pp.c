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

const char* name_of_register(int value) {
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
        return "irp";
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
    return printf("halt");
}

int pp_ret(const instruction_t i) {
    return printf("ret");
}

int pp_syscall(const instruction_t i) {
    return printf("syscall");
}

int pp_ccall(const instruction_t i) {
    return printf("cc");
}


/// print any mv
/// 0 -> mv
/// 1 -> mvng
/// 2 -> mvnt
int pp_mv_(const instruction_t i, int which) {
    const char* ni = which == 0 ? "mv" : (which == 1) ? "mvng" : (which == 2) ? "mvnt" : "unknown";
    const char* dst = name_of_register(i >> 22);
    bool_t is_register = (i >> 21) & 1;
    if (is_register) {
        const char* src = name_of_register(i >> 16);
        printf("%s %s %s", ni, dst, src);
    } else {
        int64_t value = sext21(i);
        printf("%s %s %lld", ni, dst,  (long long int) value);
    }
    return 0;
}