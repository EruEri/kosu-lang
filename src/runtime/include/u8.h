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

#ifndef __KOSU_U8_H__
#define __KOSU_U8_H__

#include <stdint.h>

uint8_t kosu_u8_add(uint8_t, uint8_t);
uint8_t kosu_u8_sub(uint8_t, uint8_t);
uint8_t kosu_u8_mult(uint8_t, uint8_t);
uint8_t kosu_u8_mod(uint8_t, uint8_t);
uint8_t kosu_u8_shiftleft(uint8_t, uint8_t);
uint8_t kosu_u8_shiftright(uint8_t, uint8_t);
uint8_t kosu_u8_not(uint8_t);

#endif