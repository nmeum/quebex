// SPDX-FileCopyrightText: 2024 University of Bremen
// SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
//
// SPDX-License-Identifier: MIT AND GPL-3.0-only

#include <stddef.h>

#define MAX 50

extern void quebex_make_symbolic(void *, size_t, size_t, const char *);

_Noreturn void __assert_fail(void) {
	__builtin_unreachable();
}

#define assert(x) \
	((void)((x) || (__assert_fail(),0)))

static int
first_divisor(int a)
{
	int i;

	for (i = 2; i < a; i++) {
		if (a % i == 0) {
			return i;
		}
	}

	return a;
}

int main(void) {
	int a;
	quebex_make_symbolic(&a, 1, sizeof(a), "prime");

	if (a <= MAX) {
		if (a > 1 && first_divisor(a) == a) {
			assert(a != 43);
			return 1;
		} else {
			return 0;
		}
	}

	return 0;
}
