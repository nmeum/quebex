// SPDX-FileCopyrightText: 2017-2020 TheAlgorithms and contributors
// SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
//
// SPDX-License-Identifier: GPL-3.0-only

#include <stddef.h>

// Taken from: https://github.com/TheAlgorithms/C/blob/10d006c3b10340b901860e4810d2122b10e35b76/sorting/insertion_sort.c

extern void quebex_symbolic_array(void *, size_t, size_t, const char *);

#define MAX 7

void insertionSort(unsigned char *arr, int size)
{
    for (int i = 1; i < size; i++)
    {
        int j = i - 1;
        unsigned char key = arr[i];
        /* Move all elements greater than key to one position */
        while (j >= 0 && key < arr[j])
        {
            arr[j + 1] = arr[j];
            j = j - 1;
        }
        /* Find a correct position for key */
        arr[j + 1] = key;
    }
}

int
main(void)
{
    unsigned char array[MAX];

    quebex_symbolic_array(array, MAX, sizeof(unsigned char), "array");
    insertionSort(array, MAX);

    return 0;
}
