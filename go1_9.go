// Copyright 2014-2016 The Zurichess Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build go1.9

package board

import "math/bits"

// logN returns the logarithm of n, where n is a power of two.
func logN(n uint64) uint {
	return uint(bits.TrailingZeros64(n)) & 0x3f
}

// popcnt counts number of bits set in n.
func popcnt(x uint64) int32 {
	return int32(bits.OnesCount64(x))
}
