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

int64_t sextn(instruction_t instruction, int n) {
    const int64_t mask_not_n = (-1) << n; 
    const uint32_t litteral = instruction & (~mask_not_n);
    if (is_set(instruction, mask_bit((n - 1)))) {
        const int64_t i = mask_not_n | instruction;
        return i;
    } else {
        return litteral;
    }
}

int64_t sext25(instruction_t instruction) {
    return sextn(instruction, 25);
}

int64_t sext22(instruction_t instruction) {
    return sextn(instruction, 22);
}

int64_t sext21(instruction_t instruction) {
    return sextn(instruction, 21);
}

int64_t sext18(instruction_t instruction) {
    return sextn(instruction, 18);
}

int64_t sext16(instruction_t instruction) {
    return sextn(instruction, 16);
}


int64_t sext15(instruction_t instruction) {
    return sextn(instruction, 15);
}

int64_t sext13(instruction_t instruction) {
    return sextn(instruction, 13);
}