// Copyright 2014-2017 The Zurichess Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package board

import "testing"

func TestPassedPawns(t *testing.T) {
	data := []struct {
		us     Color
		ours   Bitboard
		theirs Bitboard
		want   Bitboard
	}{
		{Black, 0x14020000, 0x8040200, 0x10000000},
		{Black, 0x41800000000000, 0x80600000, 0x1000000000000},
		{Black, 0x54000000000000, 0xaa000000000000, 0x54000000000000},
		{Black, 0x60800200000000, 0x42008c000, 0x200000000},
		{Black, 0x60900200000000, 0x2008c000, 0x200000000},
		{Black, 0x800000000000, 0x208012000, 0x800000000000},
		{Black, 0xa1420010000000, 0x4c300, 0x10000000},
		{Black, 0xa3400010000000, 0xec700, 0x0},
		{Black, 0xc5040008000000, 0x22408300, 0x8000000},
		{White, 0x20801a000, 0x21820000000000, 0x8000000},
		{White, 0x4000000608000, 0x1802000000000, 0x4000000000000},
		{White, 0x40800000, 0x30000000000000, 0x800000},
		{White, 0x800e300, 0x81422000000000, 0x8000000},
		{White, 0x8040200, 0x14020000, 0x8000000},
		{White, 0xa0001a000, 0x61024000000000, 0x800000000},
		{White, 0xaa000000000000, 0x54000000000000, 0xaa000000000000},
	}

	for i, d := range data {
		pos := &Position{}
		pos.ByFigure[Pawn] = d.ours | d.theirs
		pos.ByColor[d.us] = d.ours
		pos.ByColor[d.us.Opposite()] = d.theirs
		got := PassedPawns(pos, d.us)
		if d.want != got {
			t.Errorf("#%d wanted PassedPawns(%s, %x, %x) == %x, got %x",
				i, d.us, d.ours, d.theirs, d.want, got)
		}
	}
}

func TestIsolatedPawns(t *testing.T) {
	data := []struct {
		pawns Bitboard
		want  Bitboard
	}{
		{0x22408300, 0x0},
		{0x12408300, 0x10000000},
		{0x8100001818000081, 0x8100000000000081},
	}

	for i, d := range data {
		pos := &Position{}
		pos.ByFigure[Pawn] = d.pawns
		pos.ByColor[White] = d.pawns
		got := IsolatedPawns(pos, White)
		if d.want != got {
			t.Errorf("#%d wanted IsolatedPawns(%x) == %x, got %x",
				i, d.pawns, d.want, got)
		}
	}
}

func TestConnectedPawns(t *testing.T) {
	data := []struct {
		pawns Bitboard
		want  Bitboard
	}{
		{0x22408300, 0x20408300},
		{0x12408300, 0x408300},
		{0x8100001818000081, 0x1818000000},
	}

	for i, d := range data {
		pos := &Position{}
		pos.ByFigure[Pawn] = d.pawns
		pos.ByColor[White] = d.pawns
		got := ConnectedPawns(pos, White)
		if d.want != got {
			t.Errorf("#%d wanted ConnectedPawns(%x) == %x, got %x",
				i, d.pawns, d.want, got)
		}
	}
}

func TestDoubledPawns(t *testing.T) {
	data := []struct {
		us   Color
		ours Bitboard
		want Bitboard
	}{
		{White, 0x8000002020200888, 0x20200008},
		{Black, 0x8000002020200888, 0x2020000800},
		{Black, 0x45040800000000, 0x4000000000000},
		{White, 0x4604300, 0x4000},
		{Black, 0x125100000000, 0x100000000000},
		{White, 0x604100, 0x4000},
		{Black, 0xc5040000000000, 0x4000000000000},
		{Black, 0x85044010000000, 0x4000000000000},
		{White, 0x444300, 0x4000},
		{Black, 0x20481101000000, 0x100000000},
		{Black, 0x20481101000000, 0x100000000},
		{Black, 0x1800000000000, 0x0},
		{Black, 0x2180100000000, 0x0},
		{White, 0x20208000, 0x200000},
		{White, 0x5400200, 0x0},
	}

	for i, d := range data {
		pos := &Position{}
		pos.ByFigure[Pawn] = d.ours
		pos.ByColor[d.us] = d.ours
		got := DoubledPawns(pos, d.us)
		if d.want != got {
			t.Errorf("#%d wanted DoubledPawns(%v, %x) == %x, got %x",
				i, d.us, d.ours, d.want, got)
		}
	}
}

func TestPawnThreats(t *testing.T) {
	data := []struct {
		us   Color
		ours Bitboard
		want Bitboard
	}{
		{White, 0x10200, 0x2050000},
		{White, 0x800010200, 0x140002050000},
		{Black, 0x800010200, 0x14000205},
		{Black, 0x7800010200, 0xfc000205},
	}

	for i, d := range data {
		pos := &Position{}
		pos.ByFigure[Pawn] = d.ours
		pos.ByColor[d.us] = d.ours
		got := PawnThreats(pos, d.us)
		if d.want != got {
			t.Errorf("#%d wanted PawnThreats(%v, %x) == %x, got %x",
				i, d.us, d.ours, d.want, got)
		}
	}
}

func TestBackwardPawns(t *testing.T) {
	data := []struct {
		us     Color
		ours   Bitboard
		theirs Bitboard
		want   Bitboard
	}{
		{Black, 0x201000000000, 0x10000000, 0x200000000000},    // wikipedia
		{White, 0x2a0410000, 0x402a000000000, 0x400000},        // chessprogramming
		{Black, 0x402a000000000, 0x2a0014000, 0x4000000000000}, // chessprogramming
		{White, 0x10001000, 0x80008000000, 0x0},
		{White, 0x10201000, 0x80008000000, 0x0},
		{White, 0x2010001000, 0x80008000000, 0x10000000},
		{Black, 0x55aa0000000000, 0x2a00000000, 0x55000000000000},
	}

	for i, d := range data {
		pos := &Position{}
		pos.ByFigure[Pawn] = d.ours | d.theirs
		pos.ByColor[d.us] = d.ours
		pos.ByColor[d.us.Opposite()] = d.theirs
		got := BackwardPawns(pos, d.us)
		if d.want != got {
			t.Errorf("#%d wanted BackwardPawns(%s, %x, %x) == %x, got %x",
				i, d.us, d.ours, d.theirs, d.want, got)
		}
	}
}
