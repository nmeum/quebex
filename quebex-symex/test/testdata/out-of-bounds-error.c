// SPDX-FileCopyrightText: 2026 Reliable System Software, Technische Universität Braunschweig <vss@ibr.cs.tu-bs.de>
//
// SPDX-License-Identifier: GPL-3.0-only

#include <stddef.h>

extern void quebex_make_symbolic(void *, size_t, size_t, const char *);

int main(void) {
	int a;
	int buf[5];
	quebex_make_symbolic(&a, 1, sizeof(a), "a");

	if (a == 42) {
		return 1;
	} else if (a == 0x23523929) {
		return buf[1024];
	} else if (a == 1337) {
		return 42;
	}

	return 0;
}
