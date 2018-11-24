// Copyright 2014-2016 The Zurichess Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package board

import (
	"testing"
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

func TestPositionFromIncompleteFEN(t *testing.T) {
	fen := "4k3/8/8/8/8/8/4N3/4KB2 w - - 0 1"
	incomplete := "4k3/8/8/8/8/8/4N3/4KB2 w - -"
	pos, err := PositionFromFEN(incomplete)
	if err != nil {
		t.Error("Expected no error got:", err)
		return
	}
	if got := pos.String(); got != fen {
		t.Errorf("Got fen %s, expected %s", got, fen)
	}
	if got := pos.HalfmoveClock(); got != 0 {
		t.Errorf("Got half move clock %d, wanted %d", got, 0)
	}
	if got := pos.FullmoveCounter(); got != 1 {
		t.Errorf("Got fullmove counter %d, wanted %d", got, 1)
	}
}

func BenchmarkPositionFromFEN(b *testing.B) {
	for i := 0; i < b.N; i++ {
		for _, d := range TestFENs {
			PositionFromFEN(d)
		}
	}
}
