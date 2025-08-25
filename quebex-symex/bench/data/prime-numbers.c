// SPDX-FileCopyrightText: 2024 University of Bremen
// SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
//
// SPDX-License-Identifier: MIT AND GPL-3.0-only

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

unsigned
entry(unsigned a)
{
	if (a <= 35) {
		if (a > 1 && first_divisor(a) == a) {
			return 1;
		} else {
			return 0;
		}
	}

	return 0;
}
