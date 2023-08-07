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
#include "kosuvm_util.h"
#include "kosuvm_pp.h"
#include "util.h"
#include <ffi/ffi.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <dlfcn.h>


dl_handlers_t dll_error(void** handler, size_t count) {
    for (size_t i = count; i < count; i--) {
        dlclose(handler[i]);
    }

    dl_handlers_t handlers = {.count = 0};
    return handlers;
}

dl_handlers_t handle_dll(const char** librairies) {
    dl_handlers_t handlers = {.count = 0};
    if (!librairies) return handlers;

    size_t count = 0;
    for (count = 0; count < NB_DYNLIB; count += 1) {
        const char* library = *(librairies + count);
       
        if (!library) break;
        void* handler = dlopen(library, RTLD_LAZY);
        if (!handler) {
            return dll_error(handlers.handlers, count);
        }
        handlers.handlers[count] = handler;
    }
    handlers.count = count;
    return handlers;
}

kosuvm_t* kosuvm_init(int argc, const char** argv, const instruction_t *const code, uint64_t stack_size, 
    uint64_t offset, ccall_entries_t entries, const char** librairies
    ) 
{
    dl_handlers_t handlers = handle_dll(librairies);
    kosuvm_t* vm_ptr = malloc(sizeof(kosuvm_t));
    if (!vm_ptr) failwith("Vm alloc fail", 1);
    kosuvm_stack_t* stack = kosuvm_stack_create(stack_size);
    const instruction_t* ip = code + offset;
    kosuvm_t vm = {
        .stack = stack,
        .argv = argv,
        .dl_handlers = handlers,
        .cc_entries = entries,
        .code = code, 
        .ip = ip, 
        .fp = stack->sp, 
        .last_cmp = false
    };
    vm.r0 = argc;
    vm.r1 = (size_t) (void *) argv;
    // show_status(&vm);
    memcpy(vm_ptr, &vm, sizeof(kosuvm_t));
    return vm_ptr;
}

instruction_t fetch_instruction(kosuvm_t* vm) {
    return *(vm->ip++);
}


reg_t* register_of_int32(kosuvm_t* vm, uint32_t bits, uint32_t shift) {
    switch ((bits >> shift) & REG_ONLY_MASK) {
    case 0:
        return &vm->r0;
    case 1:
        return &vm->r1;
    case 2:
        return &vm->r2;
    case 3:
        return &vm->r3; 
    case 4:
        return &vm->r4; 
    case 5:
        return &vm->r5; 
    case 6:
        return &vm->r6; 
    case 7:
        return &vm->r7; 
    case 8:
        return &vm->r8;
    case 9:
        return &vm->r9;
    case 10:
        return &vm->r10;
    case 11:
        return &vm->r11;
    case 12:
        return &vm->r12;
    case 13:
        return &vm->r13;
    case 14:
        return &vm->r14;
    case 15:
        return &vm->fr0; 
    case 16:
        return &vm->fr1; 
    case 17:
        return &vm->fr2; 
    case 18:
        return &vm->fr3; 
    case 19:
        return &vm->fr4; 
    case 20:
        return &vm->fr5; 
    case 21:
        return &vm->fr6; 
    case 22:
        return &vm->fr7; 
    case 23:
        return &vm->fr8;
    case 24:
        return &vm->fr9;
    case 25:
        return &vm->fr10;
    case 26:
        return &vm->irp;
    case 27:
        return &vm->scp;
    case 28:
        return &vm->fp;
    case 29:
        return &vm->rap;
    case 30:
        return &vm->stack->sp;
    default:
        failwith("Wrong register number", 1);
    }
    return (void *) 0;
}

int iccall(kosuvm_t* vm, instruction_t instruction);

int isyscall(kosuvm_t* vm, instruction_t instruction) {

    #ifdef __APPLE__
        // Find a way since [syscall] is deprecated on macOS and __syscall doesnt exist
        // Maybe inline asm for x86_64 and arm64 
        vm->r0 = -1;
    #else
        #ifdef __FreeBSD__
            vm->r0 = __syscall(vm->scp, vm->r0, vm->r1, vm->r2, vm->r3, vm->r4, vm->r5);
        #else
            vm->r0 = syscall(vm->scp, vm->r0, vm->r1, vm->r2, vm->r3, vm->r4, vm->r5);
        #endif
    #endif
    return 0;
}

int halt_opcode(kosuvm_t* vm, instruction_t instruction, bool_t* halt) {
    if (halt) *halt = false;
    switch ((instruction >> 25) & 0x3) {
        case HALT: {
            if (halt) *halt = true;
            return 0;
        }
        case RET_BITS: {
            vm->ip = (const instruction_t *) vm->rap;
            return 0;
        }
        case SYSCALL_BITS: {
            return isyscall(vm, instruction);
        }

        case CALL_BITS: {
            return iccall(vm, instruction);
        }
    }

    return -1;
} 

void* cc_find_symbol(kosuvm_t* vm, const char* name) {
    void* fn_ptr = NULL;
    for (size_t index = 0; index < vm->dl_handlers.count; index += 1) {
        void* handler = vm->dl_handlers.handlers[index];
        fn_ptr = dlsym(handler, name);
        if (fn_ptr) break;
    }

    return fn_ptr;
}

void* iccall_offset(kosuvm_t* vm, arg_t addr, size_t* nb_args) {
    void* value = NULL;
    switch (addr.tag) {
    case AT_ADDR: {
        address_offset_t addr_off = addr.offset.o_addr_off;
        reg_t* base_reg = register_of_int32(vm, addr_off.aot_base_reg_addr, 0);
        int64_t offset = 
            (addr_off.aot_tag == AOT_REG)
            ? *register_of_int32(vm, addr_off.aot_val.aot_enc_reg, 0)
            : addr_off.aot_val.value
        ;
        int64_t raw_address = *base_reg + offset;
        return (void**) raw_address;
    }
    case AT_VALUE: {
        // printf("value = %lld\n", addr.offset.o_value);
        // fflush(stdout);
        reg_t* reg = register_of_int32(vm, *nb_args, 0);
        *reg = addr.offset.o_value;
        *nb_args = *nb_args + 1;
        return reg;
    }

    case AT_PC_REL: {
        const instruction_t* new_ip = vm->ip - 1; 
        const instruction_t* label = new_ip + addr.offset.o_pcrel;
        // Sagfaut: need other indirection;
        // Tmp fix: use register as indirection
        reg_t* reg = register_of_int32(vm, *nb_args, 0);
        *reg = (int64_t) label;
        *nb_args = *nb_args + 1;
        return (void **) reg;
    }
    }
    return value;
}

int iccall(kosuvm_t* vm, instruction_t instruction) {
    bool_t is_register = is_set(instruction, mask_bit(24));
    size_t index = (is_register) 
        ? *register_of_int32(vm, instruction, 19) 
        : sext25(instruction);
        ;
    ccall_entry_t entry = vm->cc_entries.entries[index];
    void* fn_ptr = cc_find_symbol(vm, entry.function_name);
    if (!fn_ptr) {
        fprintf(stderr, "Cannot find %s\n", entry.function_name);
        return -1;
    }

    
    ffi_cif cif;
    int status;
    if (entry.args.p_count != entry.arity) {
        // DO VARIADIC
        // printf("arity = %u, count = %lu\n", entry.arity, entry.args.p_count);
        status = ffi_prep_cif_var(
            &cif, FFI_DEFAULT_ABI, 
            entry.arity, entry.args.p_count, 
            entry.return_type, entry.args_types
            );
    } else {
        status = ffi_prep_cif(
            &cif, FFI_DEFAULT_ABI, 
            entry.arity, 
            entry.return_type, 
            entry.args_types
        );
    }

    if (status != FFI_OK) {
        fprintf(stderr, "FFI NOT OK\n");
        return -1;
    }

    // TMP Check later if non stacked args overflow on other register

    void** values = malloc(sizeof(void*) * (entry.args.p_count + 1));
    if (!values) return -1;

    // Since R0 will hold the return address
    size_t reg_ref = 1;
    for (size_t index = 0; index < entry.args.p_count; index += 1) {
        arg_t addr = entry.args.p_address[index];
        void* args_loc = iccall_offset(vm, addr, &reg_ref);
        values[index] = args_loc;
    }
    values[entry.args.p_count] = NULL;

    // show_status(vm);

    // int (*p)(const char*) = fn_ptr;
    // p("Hello world");
    ffi_call(&cif, fn_ptr, (void*) vm->r0, values);

    // show_status(vm);
    // printf("value ro = %d\n", *(int*) vm->r0 );

    free(values);
    return 0;
}

int mvnt(kosuvm_t* vm, instruction_t instruction) {
    reg_t* dst = register_of_int32(vm, instruction, 22);
    bool_t is_register = (instruction >> 21) & 1;
    if (is_register) {
        reg_t* src = register_of_int32(vm, instruction, 16);
        *dst = ~(*src);
    } else {
        int64_t value = sext21(instruction);
        *dst = ~value;
    }
    return 0;
}

int mvng(kosuvm_t* vm, instruction_t instruction) {
    reg_t* dst = register_of_int32(vm, instruction, 22);
    bool_t is_register = (instruction >> 21) & 1;
    if (is_register) {
        reg_t* src = register_of_int32(vm, instruction, 16);
        *dst = -(*src);
    } else {
        int64_t value = sext21(instruction);
        *dst = -value;
    }
    return 0;
}

int mv(kosuvm_t* vm, instruction_t instruction) {
    reg_t* dst = register_of_int32(vm, instruction, 22);
    bool_t is_register = (instruction >> 21) & 1;
    if (is_register) {
        reg_t* src = register_of_int32(vm, instruction, 16);
        *dst = (*src);
    } else {
        int64_t value = sext21(instruction);
        *dst = value;
    }
    return 0;
}

int mva(kosuvm_t* vm, instruction_t instruction) {
    reg_t* dst = register_of_int32(vm, instruction, 22);
    uint8_t shift = ((instruction >> 20) & SHIFT_ONLY_MASK) * 16;
    bool_t is_reg = is_set(instruction, mask_bit(19));
    if (is_reg) {
        reg_t* src = register_of_int32(vm, instruction, 14);
        *dst = *dst | ( *src << shift);
    } else {
        int64_t value = sext18(instruction);
        *dst = *dst | (value << shift);
    } 
    return 0;
}

int br(kosuvm_t* vm, instruction_t instruction) {
    if (!vm->last_cmp) {return 0;}
    bool_t is_branch_link = is_set(instruction, mask_bit(26));
    bool_t is_register = is_set(instruction, mask_bit(25));
    if (is_register) {
       reg_t* src = register_of_int32(vm, instruction, 20);
       if (is_branch_link) {
            vm->rap = (reg_t) vm->ip;
       }
       vm->ip = vm->code + *src;
    } else {
        int64_t value = sext25(instruction);
        if (is_branch_link) {
            vm->rap = (reg_t) vm->ip;
        }
        vm->ip = (vm->ip - 1) + value;
    }

    return 0;
}


int lea(kosuvm_t* vm, instruction_t instruction) {
    reg_t* dst = register_of_int32(vm, instruction, 22);
    bool_t is_address = is_set(instruction, mask_bit(21));
    if (is_address) {
        reg_t* reg_base_a = register_of_int32(vm, instruction, 16);
        bool_t is_value = is_set(instruction, mask_bit(15));
        int64_t value = (is_value)
            ? sext15(instruction)
            : *register_of_int32(vm, instruction, 10)
        ;
        *dst = *reg_base_a + value;
    } else {
        int64_t value = sext21(instruction);
        *dst = (reg_t) (instruction_t*) ((vm->ip - 1) + value);
        // printf("addr = %lld\n", *dst);
        // fflush(stdout);
        // printf("dst = %s\n", (const char*) *dst);
    }

    return 0;
}

#define regvalue(mesa, shift) \
    printf("reg %s = %d\n", mesa, ((instruction >> shift) & REG_ONLY_MASK)) 

int add(kosuvm_t* vm, instruction_t instruction) {
    reg_t* dst = register_of_int32(vm, instruction, 22);
    reg_t* src = register_of_int32(vm, instruction, 17);
    bool_t is_register = is_set(instruction, mask_bit(16));
    if (is_register) {
        reg_t* src2 = register_of_int32(vm, instruction, 11);
        // regvalue("src2", 11);
        // printf("src = %lld\n", *src2);
        // printf("dst = %lld\n", *dst);
        *dst = *src + *src2;
    } else {
        int64_t value = sext16(instruction);
        *dst = *src + value;
    }
    return 0;
}


int sub(kosuvm_t* vm, instruction_t instruction) {
    reg_t* dst = register_of_int32(vm, instruction, 22);
    reg_t* src = register_of_int32(vm, instruction, 17);
    bool_t is_register = is_set(instruction, mask_bit(16));
    if (is_register) {
        reg_t* src2 = register_of_int32(vm, instruction, 11);
        *dst = *src - *src2;
    } else {
        int64_t value = sext16(instruction);
        // printf("value = %lld\n", value);
        *dst = *src - value;
    }
    return 0;
}

int mult(kosuvm_t* vm, instruction_t instruction) {
    reg_t* dst = register_of_int32(vm, instruction, 22);
    reg_t* src = register_of_int32(vm, instruction, 17);
    bool_t is_register = is_set(instruction, mask_bit(16));
    if (is_register) {
        reg_t* src2 = register_of_int32(vm, instruction, 11);
        *dst = *src * *src2;
    } else {
        int64_t value = sext16(instruction);
        *dst = *src * value;
    }
    return 0;
}

int idiv(kosuvm_t* vm, instruction_t instruction) {
    return -1;
}

int mod(kosuvm_t* vm, instruction_t instruction) {
    return -1;
}


int iand(kosuvm_t* vm, instruction_t instruction) {
    reg_t* dst = register_of_int32(vm, instruction, 22);
    reg_t* src = register_of_int32(vm, instruction, 17);
    bool_t is_register = is_set(instruction, mask_bit(16));
    if (is_register) {
        reg_t* src2 = register_of_int32(vm, instruction, 11);
        *dst = *src & *src2;
    } else {
        int64_t value = sext16(instruction);
        *dst = *src & value;
    }
    return 0;
}

int ior(kosuvm_t* vm, instruction_t instruction) {
    reg_t* dst = register_of_int32(vm, instruction, 22);
    reg_t* src = register_of_int32(vm, instruction, 17);
    bool_t is_register = is_set(instruction, mask_bit(16));
    if (is_register) {
        reg_t* src2 = register_of_int32(vm, instruction, 11);
        *dst = *src | *src2;
    } else {
        int64_t value = sext16(instruction);
        *dst = *src | value;
    }
    return 0;
}

int ixor(kosuvm_t* vm, instruction_t instruction) {
    reg_t* dst = register_of_int32(vm, instruction, 22);
    reg_t* src = register_of_int32(vm, instruction, 17);
    bool_t is_register = is_set(instruction, mask_bit(16));
    if (is_register) {
        reg_t* src2 = register_of_int32(vm, instruction, 11);
        *dst = *src ^ *src2;
    } else {
        int64_t value = sext16(instruction);
        *dst = *src ^ value;
    }
    return 0;
}

int ilsl(kosuvm_t* vm, instruction_t instruction) {
    reg_t* dst = register_of_int32(vm, instruction, 22);
    reg_t* src = register_of_int32(vm, instruction, 17);
    bool_t is_register = is_set(instruction, mask_bit(16));
    if (is_register) {
        reg_t* src2 = register_of_int32(vm, instruction, 11);
        *dst = *src << *src2;
    } else {
        int64_t value = sext16(instruction);
        *dst = *src << value;
    }
    return 0;
}

int iasr(kosuvm_t* vm, instruction_t instruction) {
    reg_t* dst = register_of_int32(vm, instruction, 22);
    reg_t* src = register_of_int32(vm, instruction, 17);
    bool_t is_register = is_set(instruction, mask_bit(16));
    if (is_register) {
        reg_t* src2 = register_of_int32(vm, instruction, 11);
        *dst = ((int64_t) *src) >> *src2;
    } else {
        int64_t value = sext16(instruction);
        *dst = ((int64_t) *src) >> value;
    }
    return 0;
}

int ilsr(kosuvm_t* vm, instruction_t instruction) {
    reg_t* dst = register_of_int32(vm, instruction, 22);
    reg_t* src = register_of_int32(vm, instruction, 17);
    bool_t is_register = is_set(instruction, mask_bit(16));
    if (is_register) {
        reg_t* src2 = register_of_int32(vm, instruction, 11);
        *dst = ((uint64_t) *src) >> *src2;
    } else {
        int64_t value = sext16(instruction);
        *dst = ((uint64_t) *src) >> value;
    }
    return 0;
}

bool_t cmp_value(condition_code_t cc, reg_t lhs, reg_t rhs) {
    switch (cc) {
    case ALWAYS:
        return true;
    case EQUAL: {
        bool_t c = lhs == rhs;
        // printf("%lld == %lld: %d\n", lhs, rhs, c);
        return c;
    }
    case DIFF:
        return lhs != rhs;
    case SUP:
        return (int64_t) lhs > (int64_t) rhs;
    case UNSIGNED_SUP:
        return lhs > rhs;
    case SUPEQ:
        return (int64_t) lhs >= (int64_t) rhs;
    case UNSIGNED_SUPEQ:
        return lhs >= rhs;
    case INF:
        return (int64_t) lhs < (int64_t) rhs;
    case UNSIGNED_INF:
        return lhs < rhs;
    case INFEQ:
        return (int64_t) lhs <= (int64_t) rhs;
    case UNSIGNED_INFEQ:
        return lhs <= rhs;
    default:
        return false;
    }
}

int cmp(kosuvm_t* vm, instruction_t instruction) {
    condition_code_t cc = (instruction >> 23) & CC_ONLY_MASK;
    bool_t is_cset = is_set(instruction, mask_bit(22));
    reg_t* reg1 = register_of_int32(vm, instruction, 17);
    reg_t* reg2 = register_of_int32(vm, instruction, 12);
    bool_t value = cmp_value(cc, *reg1, *reg2);
    if (is_cset) {
        reg_t* reg3 = register_of_int32(vm, instruction, 7);
        *reg3 = value;
        if (is_set(instruction, mask_bit(6))) {
            vm->last_cmp = value;
        }
    } else {
        vm->last_cmp = value;
    }

    return 0;
}

int ldr(kosuvm_t* vm, instruction_t instruction) {
    data_size_t ds = (instruction >> 24) & DATA_SIZE_MASK;
    reg_t* dst = register_of_int32(vm, instruction, 19);
    reg_t* base = register_of_int32(vm, instruction, 14);
    bool_t is_offset_reg = is_set(instruction, mask_bit(13));
    int64_t offset = is_offset_reg 
        ? *register_of_int32(vm, instruction, 12) 
        : sext13(instruction);
    int64_t raw_address = *base + offset;
    switch (ds) {
    case S8:
        *dst = *((uint8_t*) raw_address);
        break;
    case S16:
        *dst = *((uint16_t*) raw_address);
        break;
    case S32:
        *dst = *((uint32_t*) raw_address);
        break;
    case S64:
        *dst = *((uint64_t*) raw_address);
      break;
    }

    return 0;
}

int str(kosuvm_t* vm, instruction_t instruction) {
    data_size_t ds = (instruction >> 24) & DATA_SIZE_MASK;
    reg_t* src = register_of_int32(vm, instruction, 19);
    reg_t* base = register_of_int32(vm, instruction, 14);
    bool_t is_offset_reg = is_set(instruction, mask_bit(13));
    int64_t offset = is_offset_reg 
        ? *register_of_int32(vm, instruction, 12) 
        : sext13(instruction);
    // printf("reg = %d\noffset = %lld\n", is_offset_reg, offset);
    int64_t raw_address = *base + offset;
    switch (ds) {
    case S8:
        *((uint8_t*) raw_address) = (uint8_t) *src;
        break;
    case S16:
        *((uint16_t*) raw_address) = (uint16_t) *src;
        break;
    case S32:
        *((uint32_t*) raw_address) = (uint32_t) *src;
        break;
    case S64:
        *((uint64_t*) raw_address) = (uint64_t) *src;
        // if (src == &vm->irp) {
        //     const char** s = (void**) *base + offset;
        //     printf("addr = %p\n", s);
        //     printf("in str = %s\n", *s);
        // }
        break;
    }

    return 0;
}

#define mftoi(value) \
    bits_of_double((double) (value))

#define mitof(type, value) \
    (type) (value)

int ftoi(kosuvm_t* vm, instruction_t instruction) {
    bool_t is_signed = is_set(instruction, mask_bit(25));
    data_size_t ds = (instruction >> 23) & DATA_SIZE_MASK;
    reg_t* dst = register_of_int32(vm, instruction, 18);
    reg_t* src = register_of_int32(vm, instruction, 13);
    switch (ds) { 
        case S8: {
            if (is_signed) {
                *dst = mftoi((int8_t) *src);
            } else {
                *dst = mftoi((uint8_t) *src);
            }
            break;
        }
        case S16:{
            if (is_signed) {
                *dst = mftoi((int16_t) *src);
            } else {
                *dst = mftoi((uint16_t) *src);
            }
            break;
        }
        case S32:{
            if (is_signed) {
                *dst = mftoi((int32_t) *src);
            } else {
                *dst = mftoi((uint32_t) *src);
            }
            break;
        }
        case S64: {
            if (is_signed) {
                *dst = mftoi((int64_t) *src);
            } else {
                *dst = mftoi((uint64_t) *src);
            }
            break;
        }
        break; 
    }
    return 0;
}

int itof(kosuvm_t* vm, instruction_t instruction) {
    bool_t is_signed = is_set(instruction, mask_bit(25));
    data_size_t ds = (instruction >> 23) & DATA_SIZE_MASK;
    reg_t* dst = register_of_int32(vm, instruction, 18);
    reg_t* rsrc = register_of_int32(vm, instruction, 13);
    double src = double_of_bits(*rsrc);
    switch (ds) { 
        case S8: {
            if (is_signed) {
                *dst = mitof(int8_t, src) ;
            } else {
                *dst = mitof(uint8_t, src);
            }
            break;
        }
        case S16:{
            if (is_signed) {
                *dst = mitof(int16_t, src) ;
            } else {
                *dst = mitof(uint16_t, src);
            }
            break;
        }
        case S32:{
            if (is_signed) {
                *dst = mitof(int32_t, src) ;
            } else {
                *dst = mitof(uint32_t, src);
            }
            break;
        }
        case S64: {
            if (is_signed) {
                *dst = mitof(int64_t, src) ;
            } else {
                *dst = mitof(uint64_t, src);
            }
            break;
        }
        break; 
    }
    return 0;
}

int ldr_str(kosuvm_t* vm, instruction_t instruction) {
    bool_t is_str = is_set(instruction, mask_bit(26));
    return is_str ? str(vm, instruction) : ldr(vm, instruction);
}

int itof_ftoi(kosuvm_t* vm, instruction_t instruction) {
    bool_t is_ftoi = is_set(instruction, mask_bit(26));
    return is_ftoi ? ftoi(vm, instruction) : itof(vm, instruction);
}

int kosuvm_run_single(kosuvm_t* vm) {
        instruction_t instruction = fetch_instruction(vm);
        //pp_instruction(instruction);
        //puts("");
        kosuvm_opcode_t ist = opcode_value(instruction);
        // printf("instruction code = %u\n", ist);
        switch (ist) { 
            case HALT: {
                bool_t b;
                halt_opcode(vm, instruction, &b);
                if (b) {
                    return VM_HALT_EXIT_CODE;
                } 
                break;
            }
            case MVNOT:
                mvnt(vm, instruction);
                break;
            case MVNEG:
                mvng(vm, instruction);
                break;
            case MOV:
                mv(vm, instruction);
                break;
            case BR_JUMP:
                br(vm, instruction);
                break;
            case LEA:
                lea(vm, instruction);
                break;
            case ADD:
                add(vm, instruction);
                break;
            case SUB:
                sub(vm, instruction);
                break;
            case MULT:
                mult(vm, instruction);
                break;
            case DIV:
                idiv(vm, instruction);
                break;
            case MOD:
                mod(vm, instruction);
                break;
            case AND:
                iand(vm, instruction);
                break;
            case OR:    
                ior(vm, instruction);
                break;
            case XOR:
                ixor(vm, instruction);
                break;
            case LSL:
                ilsl(vm, instruction);
                break;
            case ASR:
                iasr(vm, instruction);
                break;
            case LSR:
                ilsr(vm, instruction);
                break;
            case CMP_CSET:
                cmp(vm, instruction);
                break;
            case LDR_STR:
                ldr_str(vm, instruction);
                break;
            case ITOF_FTOI:
                itof_ftoi(vm, instruction);
                break;
            default:
                fprintf(stderr, "Unknown opcode %u\n", ist);
                failwith("", 1);
            break;
        }
    // show_status(vm);
    return VM_INSTR_SUCESS;
}


int kosuvm_run(kosuvm_t* vm){
    int status;
    do {
        status = kosuvm_run_single(vm);
        // show_status(vm);
    } while (status != VM_HALT_EXIT_CODE);

    return vm->r0;
}


void kosuvm_free(kosuvm_t* vm){
    kosuvm_stack_free(vm->stack);
    dll_error(vm->dl_handlers.handlers, vm->dl_handlers.count);
    free(vm);
}