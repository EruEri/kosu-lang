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


#include "../include/memory.h"
#include <stddef.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

#define nil NULL

kosu_pointer_t* KOSU_POINTERS = nil;
kosu_frame_t* KOSU_FRAME_STACK = nil;

void kosu_alloc_error() {
    // TODO: free alloc allocated memory
    printf("Kosu Error alloc\n");
    exit(1);
}


void kosu_push_frame(void** frame, size_t variable_size) {
    kosu_frame_t* kosu_frame = malloc(sizeof(kosu_frame_t));
    if (!kosu_frame) {
        kosu_alloc_error();
    }

    kosu_frame->m_root = frame;
    kosu_frame->m_frame_size = variable_size;
    kosu_frame->m_previous = KOSU_FRAME_STACK;
    KOSU_FRAME_STACK = kosu_frame;
}

void kosu_pop_frame() {
    if (!KOSU_FRAME_STACK) {
        return;
    }
    KOSU_FRAME_STACK = KOSU_FRAME_STACK->m_previous;
}


void* kosu_alloc(uint64_t size) {
    return kosu_alloc_array(size, 1);
}

void* kosu_alloc_array(uint64_t size, size_t nb_elt) {
    void* alloc = malloc(size * nb_elt);
    if (!alloc) {
        kosu_alloc_error();
    }
    kosu_pointer_t* kosu_pointer = malloc(sizeof(kosu_pointer_t));
    if (kosu_pointer) { kosu_alloc_error();}

    kosu_pointer->m_nb_alloc = nb_elt;
    kosu_pointer->m_base = alloc;
    kosu_pointer->m_nb_alloc = nb_elt;
    kosu_pointer->m_previous = KOSU_POINTERS;
    kosu_pointer->m_color = KOSU_GC_NONE;
    KOSU_POINTERS = kosu_pointer;
    return alloc;
}

bool is_kosu_pointer(void* ptr) {
    kosu_pointer_t* root = KOSU_POINTERS;
    while (root) {
        if (root->m_base == ptr) return true;
        root = root->m_previous;
    }
    return false;
}

kosu_pointer_t* kosu_find_pointer(void* pointer) {
    kosu_pointer_t* root = KOSU_POINTERS;
    while (root) {
        
    }
    return NULL;
}

void kosu_gc_mark_frame(kosu_frame_t* frame) {    
    for (size_t start = 0; start < frame->m_frame_size; start += 1) {
        void** root = frame->m_root + start;
    }
}


void kosu_gc_mark() {
    kosu_frame_t* root = KOSU_FRAME_STACK;
    while (root) {

    }
}