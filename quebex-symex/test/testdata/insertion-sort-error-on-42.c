// SPDX-FileCopyrightText: 2017-2020 TheAlgorithms and contributors
// SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
//
// SPDX-License-Identifier: GPL-3.0-only

#include <stddef.h>

// Taken from: https://github.com/TheAlgorithms/C/blob/10d006c3b10340b901860e4810d2122b10e35b76/sorting/insertion_sort.c

extern void quebex_make_symbolic(void *, size_t, size_t, const char *);

_Noreturn void __assert_fail(void) {
    __builtin_unreachable();
}

void insertionSort(int *arr, int size)
{
    for (int i = 1; i < size; i++)
    {
        int j = i - 1;
        int key = arr[i];
        /* Move all elements greater than key to one position */
        while (j >= 0 && key < arr[j])
        {
            arr[j + 1] = arr[j];
            j = j - 1;
        }
        /* Find a correct position for key */
        arr[j + 1] = key;
        if (key == 42) {
            __assert_fail();
        }
    }
}

#define INPUT_SIZE 3

int
main(void)
{
    int array[INPUT_SIZE];

    quebex_make_symbolic(array, INPUT_SIZE, sizeof(int), "array");
    insertionSort(array, INPUT_SIZE);

    return 0;
}
