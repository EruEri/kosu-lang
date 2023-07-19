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

#ifndef __KOSUVM_UTIL_H__
#define __KOSUVM_UTIL_H__


#include <stdint.h>
#include "kosuvm_base.h"

#define opcode_value(instruction) \
    (((uint32_t) instruction & KOSUVM_OPCODE_MASK) >> (KOSUVM_INSTRUCTION_SIZE - KOSUVM_OPCODE_SIZE))

#define is_set(instruction, mask) \
    ((instruction & mask) == mask)

#define mask_bit(n) \
    (1 << n)


int64_t sextn(instruction_t instruction, int n);

int64_t sext25(instruction_t instruction);

int64_t sext22(instruction_t instruction);

int64_t sext21(instruction_t instruction);

int64_t sext18(instruction_t instruction, bool_t is_signed_extend);

int64_t sext16(instruction_t instruction);

int64_t sext13(instruction_t instruction);


#endif