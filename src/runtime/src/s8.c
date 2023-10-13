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

#include "../include/s8.h"

#define binop(op, name) \
    int8_t kosu##_s8_##name(int8_t x, int8_t y) {\
        return x op y;\
    }

#define unop(op, name) \
    int8_t kosu##_s8_##name(int8_t x) {\
        return op x;\
    }

binop(+, add)
binop(-, sub)
binop(*, mult)
binop(/, div)
binop(%, mod)
binop(<<, shiftleft)
binop(>>, shiftright)
unop(-, neg)
unop(~, not)