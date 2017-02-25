// Copyright 2014-2016 The Zurichess Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package board

const (
	debrujinMul   = 0x218A392CD3D5DBF
	debrujinShift = 58
)

var (
	debrujin64 = [64]uint{
		0, 1, 2, 7, 3, 13, 8, 19, 4, 25, 14, 28, 9, 34, 20, 40,
		5, 17, 26, 38, 15, 46, 29, 48, 10, 31, 35, 54, 21, 50, 41, 57,
		63, 6, 12, 18, 24, 27, 33, 39, 16, 37, 45, 47, 30, 53, 49, 56,
		62, 11, 23, 32, 36, 44, 52, 55, 61, 22, 43, 51, 60, 42, 59, 58,
	}
)

// logN returns the logarithm of n, where n is a power of two.
func logN(n uint64) uint {
	return debrujin64[n*debrujinMul>>debrujinShift] & 63
	// &63 lets the compiler now that sq fits 6 bits and should not generate CMPQ, SBBQ, ANDQ instructions on amd64
}

const (
	k1 = 0x5555555555555555
	k2 = 0x3333333333333333
	k4 = 0x0f0f0f0f0f0f0f0f
	kf = 0x0101010101010101
)

// popcnt counts number of bits set in n.
func popcnt(x uint64) int32 {
	// Code adapted from https://chessprogramming.wikispaces.com/Population+Count.
	x = x - ((x >> 1) & k1)
	x = (x & k2) + ((x >> 2) & k2)
	x = (x + (x >> 4)) & k4
	x = (x * kf) >> 56
	return int32(x)
}
