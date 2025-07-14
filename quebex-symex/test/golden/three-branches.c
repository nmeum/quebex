// SPDX-FileCopyrightText: 2024 University of Bremen
//
// SPDX-License-Identifier: MIT

int entry(int a, int b) {
	int r;

	if (a < b) {
		if (a < 5) {
			return 3;
		} else {
			return 2;
		}
	} else {
		return 1;
	}
}
