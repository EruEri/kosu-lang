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

#ifndef __KOSU_S8_H__
#define __KOSU_S8_H__

#include <stdint.h>

int8_t kosu_s8_add(int8_t, int8_t);
int8_t kosu_s8_sub(int8_t, int8_t);
int8_t kosu_s8_mult(int8_t, int8_t);
int8_t kosu_s8_mod(int8_t, int8_t);
int8_t kosu_s8_shiftleft(int8_t, int8_t);
int8_t kosu_s8_shiftright(int8_t, int8_t);
int8_t kosu_s8_neg(int8_t);
int8_t kosu_s8_not(int8_t);

#endif