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

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "util.h"


uint64_t alignn(uint64_t size, uint64_t to) {
    uint64_t div = size / to;
    uint64_t modulo = size % to == 0 ? 0 : 1;
    return to * (div + modulo);
}

uint64_t align8(uint64_t size) {
    return alignn(size, 8);
}

uint64_t bits_of_double(double d) {
    uint64_t* ptr = (uint64_t*) &d;
    return *ptr;
}

double double_of_bits(uint64_t t){
    double* ptr = (double*) &t;
    return *ptr;
}


void failwith(const char* message, int code) {
    puts(message);
    exit(code);
}
