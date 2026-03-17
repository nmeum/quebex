// SPDX-FileCopyrightText: 2024 Nihal Jere <nihal@nihaljere.xyz>
// SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
//
// SPDX-License-Identifier: Unlicense

// Taken from https://git.sr.ht/~mcf/cproc/commit/4f206ac1ea1b20400fa242f2f3be86237c4ba3bf

int f(int i, ...) {
	int r, c = 0;
	__builtin_va_list ap;

	__builtin_va_start(ap, i);
	r = **__builtin_va_arg(ap, int (*)[++i]);
	__builtin_va_end(ap);
	return r + i;
}

int main(void) {
	int a[3];

	a[0] = 123;
	return f(3, &a);
}
