// Copyright 2014-2016 The Zurichess Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package board

import "testing"

func TestLogN(t *testing.T) {
	for e := uint(0); e < 64; e++ {
		n := uint64(1) << e
		actual := logN(n)
		if actual != e {
			t.Errorf("expected logN(%d) == %d, got %d", n, e, actual)
		}
	}
}

func TestPopcnt(t *testing.T) {
	data := []struct {
		n uint64
		r int32
	}{
		{0, 0},
		{1, 1},
		{2, 1},
		{3, 2},
		{4, 1},
		{5, 2},
		{6, 2},
		{7, 3},
		{8, 1},
		{9, 2},
		{1 << 10, 1},
		{1<<10 + 1<<12, 2},
		{1<<64 - 1, 64},
		{1<<64 - 1<<32 + 1, 33},
	}

	for i, d := range data {
		if got, want := popcnt(d.n), d.r; got != want {
			t.Errorf("#%d wanted popcnt(%d) == %d, got %d", i, d.n, want, got)
		}
	}
}
