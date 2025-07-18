// SPDX-FileCopyrightText: 2024 University of Bremen
// SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
//
// SPDX-License-Identifier: MIT AND GPL-3.0-only

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
