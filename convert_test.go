// Copyright 2014-2016 The Zurichess Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package board

import (
	"testing"

	. "bitbucket.org/zurichess/zurichess/internal/testdata"
)

func TestPositionFromFENAndBack(t *testing.T) {
	for _, d := range TestFENs {
		pos, err := PositionFromFEN(d)
		if err != nil {
			t.Errorf("%s failed with %v", d, err)
		} else if fen := pos.String(); d != fen {
			t.Errorf("expected %s, got %s", d, fen)
		}
	}
}

func BenchmarkPositionFromFEN(b *testing.B) {
	for i := 0; i < b.N; i++ {
		for _, d := range TestFENs {
			PositionFromFEN(d)
		}
	}
}
