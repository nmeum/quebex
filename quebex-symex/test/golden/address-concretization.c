// SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
//
// SPDX-License-Identifier: GPL-3.0-only

#include <assert.h>

#define LEN(X) (sizeof(X) / sizeof(X[0]))

unsigned
entry(unsigned a)
{
	int array[5] = {0};

	if (a < LEN(array)) {
		int *ptr = &array[0];
		*(ptr + a) = 42;

		unsigned tgt = 0;
		for (unsigned i = 0; i < LEN(array); i++) {
			if (array[i]) {
				tgt = i;
				break;
			}
		}

		assert(a == tgt);
	}

	return 0;
}
