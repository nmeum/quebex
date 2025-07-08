// SPDX-FileCopyrightText: 2021 Gabriel Fioravante
// SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
//
// SPDX-License-Identifier: GPL-3.0-only

// Taken from: https://github.com/TheAlgorithms/C/blob/f241de90e1691dc7cfcafcbecd89ef12db922e6b/sorting/bubble_sort_2.c

#include <stdint.h>
#include <stdbool.h>

void bubble_sort(int* array_sort, unsigned max)
{
    bool is_sorted = false;

    /* keep iterating over entire array
     * and swaping elements out of order
     * until it is sorted */
    while (!is_sorted)
    {
        is_sorted = true;

        /* iterate over all elements */
        for (int i = 0; i < max - 1; i++)
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

int rand(uint64_t *seed)
{
    *seed = 6364136223846793005ULL*(*seed) + 1; // From musl libc.
    return (*seed); // can't shift here because we don't have that
}

void fillary(int *ary, unsigned n) {
    uint64_t seed = 42;
    for (unsigned i = 0; i < n; i++) {
        ary[i] = rand(&seed);
    }
}

int entry(unsigned max) {
    int array[max];

    fillary(array, max);
    bubble_sort(array, max);

    for (unsigned i = 0; i < max-1; i++) {
        if (array[i] > array[i+1]) {
            return 1;
        }
    }

    return 0;
}

int main()
{
	entry(5000);
}
