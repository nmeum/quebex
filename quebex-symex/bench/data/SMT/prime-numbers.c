// SPDX-FileCopyrightText: 2024 University of Bremen
// SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
//
// SPDX-License-Identifier: MIT AND GPL-3.0-only

#include <stddef.h>

#define MAX 200

extern void quebex_symbolic_array(void *, size_t, size_t, const char *);

static unsigned
first_divisor(unsigned a)
{
	unsigned i;

	for (i = 2; i < a; i++) {
		if (a % i == 0) {
			return i;
		}
	}

	return a;
}

int
main(void)
{
	unsigned a;
	quebex_symbolic_array(&a, 1, sizeof(a), "a");

	if (a <= MAX) {
		if (a > 1 && first_divisor(a) == a) {
			return 1;
		} else {
			return 0;
		}
	}

	return 0;
}
