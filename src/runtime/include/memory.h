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

#ifndef __MEMORY_H__
#define __MEMORY_H__


#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct kosu_pointer kosu_pointer_t;
typedef struct kosu_frame kosu_frame_t;


typedef enum kosu_gc_color {
    KOSU_GC_NONE,
    KOSU_GC_VISITED
} kosu_gc_color_t;

struct kosu_pointer {
    char* m_base;
    size_t m_type_size;
    size_t m_nb_alloc;
    kosu_gc_color_t m_color; 
    kosu_pointer_t* m_previous;
};

struct kosu_frame {
    void** m_root;
    size_t m_frame_size;
    kosu_frame_t* m_previous;
};


void kosu_push_frame(void** frame, size_t);
void kosu_pop_frame();


void* kosu_alloc(uint64_t);
void* kosu_alloc_array(uint64_t, size_t);

#endif