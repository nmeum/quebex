// SPDX-FileCopyrightText: 2021 Gabriel Fioravante
// SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
//
// SPDX-License-Identifier: GPL-3.0-only

// Taken from: https://github.com/TheAlgorithms/C/blob/f241de90e1691dc7cfcafcbecd89ef12db922e6b/sorting/bubble_sort_2.c

#include <stddef.h>
#include <stdbool.h>
#include <assert.h>

#define MAX 6

extern void quebex_symbolic_array(void *, size_t, size_t, const char *);

void bubble_sort(int* array_sort)
{
    bool is_sorted = false;

    /* keep iterating over entire array
     * and swaping elements out of order
     * until it is sorted */
    while (!is_sorted)
    {
        is_sorted = true;

        /* iterate over all elements */
        for (int i = 0; i < MAX - 1; i++)
        {
            /* check if adjacent elements are out of order */
            if (array_sort[i] > array_sort[i + 1])
            {
                /* swap elements */
                int change_place = array_sort[i];
                array_sort[i] = array_sort[i + 1];
                array_sort[i + 1] = change_place;
                /* elements out of order were found
                 * so we reset the flag to keep ordering
                 * until no swap operations are executed */
                is_sorted = false;
            }
        }
    }
}

int
main(void)
{
    int array[MAX];

    quebex_symbolic_array(array, MAX, sizeof(int), "array");
    bubble_sort(array);

    return 0;
}
