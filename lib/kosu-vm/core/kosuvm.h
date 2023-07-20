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

#include "kosuvm_base.h"
#include "util.h"
#include <stdint.h>



kosuvm_t* kosuvm_init(instruction_t const * const code, uint64_t stack_size, uint64_t offset); 
int kosuvm_run(kosuvm_t* vm);
int kosuvm_run_single(kosuvm_t* vm);
void kosuvm_free(kosuvm_t* vm);
#endif