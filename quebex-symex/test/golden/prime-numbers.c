// SPDX-FileCopyrightText: 2024 University of Bremen
// SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
//
// SPDX-License-Identifier: MIT AND GPL-3.0-only

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

int
entry(int a)
{
	// Written in this convoluted way to avoid phi instructions,
	// which (at the time of writing) quebex doesn't support yet.
	if (a <= 50) {
		int b = a;
		if (b <= 1) {
			return 0;
		} else {
			int c = b;
			if (first_divisor(c) == c) {
				return 1;
			} else {
				return 0;
			}
		}
	}

	return 0;
}
