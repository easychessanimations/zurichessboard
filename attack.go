// Copyright 2014-2016 The Zurichess Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// attack.go generates move bitboards for all pieces.
// zurichess uses magic bitboards to generate sliding pieces moves.
// A very good description by Pradyumna Kannan can be read at:
// http://www.pradu.us/old/Nov27_2008/Buzz/research/magic/Bitboards.pdf
//
// TODO: move magic generation into an internal package.

package board

import (
	"fmt"
	"math"
	"math/bits"
	"math/rand"
)

var (
	// bbPawnAttack contains pawn's attack tables.
	bbPawnAttack [64]Bitboard
	// bbKnightAttack contains knight's attack tables.
	bbKnightAttack [64]Bitboard
	// bbKingAttack contains king's attack tables (excluding castling).
	bbKingAttack [64]Bitboard
	bbKingArea   [64]Bitboard
	// bbSuperAttack contains queen piece's attack tables. This queen can jump.
	bbSuperAttack [64]Bitboard

	rookMagic    [64]magicInfo
	rookDeltas   = [][2]int{{-1, +0}, {+1, +0}, {+0, -1}, {+0, +1}}
	bishopMagic  [64]magicInfo
	bishopDeltas = [][2]int{{-1, +1}, {+1, +1}, {+1, -1}, {-1, -1}}
)

func init() {
	initBbPawnAttack()
	initBbKnightAttack()
	initBbKingAttack()
	initBbKingArea()
	initBbSuperAttack()
	initRookMagic()
	initBishopMagic()
}

func initJumpAttack(jump [][2]int, attack []Bitboard) {
	for r := 0; r < 8; r++ {
		for f := 0; f < 8; f++ {
			bb := Bitboard(0)
			for _, d := range jump {
				r0, f0 := r+d[0], f+d[1]
				if 0 <= r0 && r0 < 8 && 0 <= f0 && f0 < 8 {
					bb |= RankFile(r0, f0).Bitboard()
				}
			}
			attack[RankFile(r, f)] = bb
		}
	}
}

func initBbPawnAttack() {
	pawnJump := [][2]int{{-1, -1}, {-1, +1}, {+1, +1}, {+1, -1}}
	initJumpAttack(pawnJump, bbPawnAttack[:])
}

func initBbKnightAttack() {
	knightJump := [][2]int{
		{-2, -1}, {-2, +1}, {+2, -1}, {+2, +1},
		{-1, -2}, {-1, +2}, {+1, -2}, {+1, +2},
	}
	initJumpAttack(knightJump, bbKnightAttack[:])
}

func initBbKingAttack() {
	kingJump := [][2]int{
		{-1, -1}, {-1, +0}, {-1, +1},
		{+0, +1}, {+0, -1},
		{+1, +1}, {+1, +0}, {+1, -1},
	}
	initJumpAttack(kingJump, bbKingAttack[:])
}

func initBbKingArea() {
	kingJump := [][2]int{
		{+1, -1}, {+1, +0}, {+1, +1},
		{+0, +1}, {+0, +0}, {+0, +1},
		{-1, -1}, {-1, +0}, {-1, +1},
	}
	initJumpAttack(kingJump, bbKingArea[:])
}

func initBbSuperAttack() {
	for sq := SquareMinValue; sq <= SquareMaxValue; sq++ {
		bbSuperAttack[sq] = slidingAttack(sq, rookDeltas, BbEmpty) | slidingAttack(sq, bishopDeltas, BbEmpty)
	}
}

func slidingAttack(sq Square, deltas [][2]int, occupancy Bitboard) Bitboard {
	r, f := sq.Rank(), sq.File()
	bb := Bitboard(0)
	for _, d := range deltas {
		for r0, f0 := r, f; ; {
			r0, f0 = r0+d[0], f0+d[1]
			if 0 > r0 || r0 >= 8 || 0 > f0 || f0 >= 8 {
				break // Stop when outside of the board.
			}
			sq0 := RankFile(r0, f0)
			bb |= sq0.Bitboard()
			if occupancy.Has(sq0) {
				break // Stop when a piece was hit.
			}
		}
	}
	return bb
}

// spell hashes bb using magic.
//
// magic stores in the upper 4 bits the shift.
// spell will return a number between 0 and 1<<shift that can be used
// to index in an array of size 1<<shift.
func spell(magic uint64, bb Bitboard) uint {
	shift := uint(magic >> 58)
	mul := magic * uint64(bb)
	return uint(mul >> shift)
}

type magicInfo struct {
	mask  Bitboard   // square's mask.
	magic uint64     // magic multiplier. first 6 bits are 64-shift.
	store []Bitboard // attack boards of size 1<<shift
}

func (mi *magicInfo) Attack(ref Bitboard) Bitboard {
	return mi.store[spell(mi.magic, ref&mi.mask)]
}

type wizard struct {
	// Sliding deltas.
	Deltas        [][2]int
	MinShift      uint // Which shifts to search.
	MaxShift      uint
	MaxNumEntries uint // How much to search.
	Rand          *rand.Rand

	numMagicTests uint
	magics        [64]uint64
	shifts        [64]uint // Number of bits for indexes.

	store     []Bitboard // Temporary store to check hash collisions.
	reference []Bitboard
	occupancy []Bitboard
}

func (wiz *wizard) tryMagicNumber(mi *magicInfo, sq Square, magic uint64) bool {
	wiz.numMagicTests++
	shift := 64 - uint(magic>>58)

	// Clear store.
	if len(wiz.store) < 1<<shift {
		wiz.store = make([]Bitboard, 1<<shift)
	}
	for j := range wiz.store[:1<<shift] {
		wiz.store[j] = 0
	}

	// Verify that magic gives a perfect hash.
	for i, bb := range wiz.reference {
		index := spell(magic, bb)
		if wiz.store[index] != 0 && wiz.store[index] != wiz.occupancy[i] {
			return false
		}
		wiz.store[index] = wiz.occupancy[i]
	}

	// Perfect hash, store it.
	wiz.magics[sq] = magic
	wiz.shifts[sq] = shift

	mi.store = make([]Bitboard, 1<<shift)
	copy(mi.store, wiz.store)
	mi.mask = wiz.mask(sq)
	mi.magic = magic
	return true
}

// randMagic returns a random magic number
func (wiz *wizard) randMagic() uint64 {
	r := uint64(wiz.Rand.Int63())
	r &= uint64(wiz.Rand.Int63())
	r &= uint64(wiz.Rand.Int63())
	return r << 1
}

// mask is the attack set on empty board minus the border.
func (wiz *wizard) mask(sq Square) Bitboard {
	// Compute border. Trick source: stockfish.
	border := (BbRank1 | BbRank8) &^ RankBb(sq.Rank())
	border |= (BbFileA | BbFileH) &^ FileBb(sq.File())
	return slidingAttack(sq, wiz.Deltas, BbEmpty) &^ border
}

// prepare computes reference and occupancy tables for a square.
func (wiz *wizard) prepare(sq Square) {
	wiz.reference = wiz.reference[:0]
	wiz.occupancy = wiz.occupancy[:0]

	// Carry-Rippler trick to enumerate all subsets of mask.
	for mask, subset := wiz.mask(sq), Bitboard(0); ; {
		attack := slidingAttack(sq, wiz.Deltas, subset)
		wiz.reference = append(wiz.reference, subset)
		wiz.occupancy = append(wiz.occupancy, attack)
		subset = (subset - mask) & mask
		if subset == 0 {
			break
		}
	}
}

func (wiz *wizard) searchSquareMagic(sq Square, mi *magicInfo) {
	if wiz.shifts[sq] != 0 && wiz.shifts[sq] <= wiz.MinShift {
		return // Don't search if shift is low enough.
	}

	// Try magic numbers with small shifts.
	wiz.prepare(sq)
	mask := wiz.mask(sq)
	for i := 0; i < 100 || wiz.shifts[sq] == 0; i++ {
		// Pick a smaller shift than current best.
		var shift uint
		if wiz.shifts[sq] == 0 {
			shift = wiz.MaxShift
		} else {
			shift = wiz.shifts[sq] - 1
		}
		if shift >= 16 {
			panic("shift too large, should fit in 4 bits")
		}

		// Pick a good magic and test whether it gives a perfect hash.
		var magic uint64
		for bits.OnesCount64(uint64(mask)*magic) < 8 {
			magic = wiz.randMagic()>>6 + uint64(64-shift)<<58
		}
		wiz.tryMagicNumber(mi, sq, magic)
	}
}

// SearchMagic finds new magics.
func (wiz *wizard) searchMagic(mi []magicInfo) {
	numEntries := uint(math.MaxUint32)
	minShift := uint(math.MaxUint32)
	for numEntries > wiz.MaxNumEntries {
		numEntries = 0
		for sq := SquareMinValue; sq <= SquareMaxValue; sq++ {
			wiz.searchSquareMagic(sq, &mi[sq])
			numEntries += 1 << wiz.shifts[sq]
			if minShift > wiz.shifts[sq] {
				minShift = wiz.shifts[sq]
			}
		}
	}
}

func (wiz *wizard) SetMagic(mi []magicInfo, sq Square, magic uint64) {
	wiz.prepare(sq)
	if !wiz.tryMagicNumber(&mi[sq], sq, magic) {
		panic(fmt.Sprintf("invalid magic: sq=%v magic=%d", sq, magic))
	}
}

func initRookMagic() {
	wiz := &wizard{
		Deltas:        rookDeltas,
		MinShift:      10,
		MaxShift:      13,
		MaxNumEntries: 130000,
		Rand:          rand.New(rand.NewSource(1)),
	}

	// A set of known good magics for rook.
	// Finding good rook magics is slow, so we just use some precomputed values.
	magics := [SquareArraySize]struct {
		square Square
		magic  uint64
	}{
		{SquareA1, 15024008494657323012}, {SquareA2, 15420465862145998980}, {SquareA3, 15420360858248675456}, {SquareA4, 15276350881835139072}, {SquareA5, 15312274192586506881}, {SquareA6, 15281135886646280192}, {SquareA7, 15278752145445224704}, {SquareA8, 14988014951518438155},
		{SquareB1, 15294259519996137476}, {SquareB2, 15564722062066090240}, {SquareB3, 15569513459967402112}, {SquareB4, 15611798496353456256}, {SquareB5, 15565003446863020672}, {SquareB6, 15569401309730734208}, {SquareB7, 15567043958411395712}, {SquareB8, 15276491966412603401},
		{SquareC1, 15312256325382045824}, {SquareC2, 15565144137081556992}, {SquareC3, 15636641393034924032}, {SquareC4, 15645507856414802208}, {SquareC5, 15576280971182350336}, {SquareC6, 15642200110987804696}, {SquareC7, 15618765395016335616}, {SquareC8, 15357283595238638210},
		{SquareD1, 15348272069860659200}, {SquareD2, 15729384789340406272}, {SquareD3, 15637906381030330368}, {SquareD4, 15600486705692942592}, {SquareD5, 15636639777598484480}, {SquareD6, 15574028057900679176}, {SquareD7, 15641005904995287232}, {SquareD8, 15276773092490225730},
		{SquareE1, 15420364777538454016}, {SquareE2, 15564721821532295425}, {SquareE3, 15742333597590290440}, {SquareE4, 15566693215813780488}, {SquareE5, 15708577492656719872}, {SquareE6, 15718143825794891828}, {SquareE7, 15569371622111969536}, {SquareE8, 15287750478839154689},
		{SquareF1, 15420334615996466192}, {SquareF2, 15709118759527645200}, {SquareF3, 15564722887284762626}, {SquareF4, 15623059677267624064}, {SquareF5, 15619046700373525552}, {SquareF6, 15574010496028901381}, {SquareF7, 15727132917522563584}, {SquareF8, 15279024892104737794},
		{SquareG1, 15276210512305588225}, {SquareG2, 15601032093541466148}, {SquareG3, 15564581599453392384}, {SquareG4, 15565020871512164417}, {SquareG5, 15564450286247383048}, {SquareG6, 15568380966301990932}, {SquareG7, 15601876767569346816}, {SquareG8, 15294376071855603844},
		{SquareH1, 15060042870528442434}, {SquareH2, 15285357877078804608}, {SquareH3, 15278534303665902595}, {SquareH4, 15276350710036316416}, {SquareH5, 15276350674812539136}, {SquareH6, 15276211312644980745}, {SquareH7, 15276211059241959936}, {SquareH8, 15060041556272906338},
	}
	for _, m := range magics {
		wiz.SetMagic(rookMagic[:], m.square, m.magic)
	}

	// Enable the next line to find new magics.
	// wiz.searchMagic(rookMagic[:])
}

func initBishopMagic() {
	wiz := &wizard{
		Deltas:        bishopDeltas,
		MinShift:      5,
		MaxShift:      9,
		MaxNumEntries: 6000,
		Rand:          rand.New(rand.NewSource(1)),
	}

	// Bishop magics, unlike rook magics are easy to find.
	wiz.searchMagic(bishopMagic[:])
}

// KnightMobility returns all squares a knight can reach from sq.
func KnightMobility(sq Square) Bitboard {
	return bbKnightAttack[sq]
}

// BishopMobility returns the squares a bishop can reach from sq given all pieces.
func BishopMobility(sq Square, all Bitboard) Bitboard {
	return bishopMagic[sq].Attack(all)
}

// RookMobility returns the squares a rook can reach from sq given all pieces.
func RookMobility(sq Square, all Bitboard) Bitboard {
	return rookMagic[sq].Attack(all)
}

// QueenMobility returns the squares a queen can reach from sq given all pieces.
func QueenMobility(sq Square, all Bitboard) Bitboard {
	return rookMagic[sq].Attack(all) | bishopMagic[sq].Attack(all)
}

// SuperQueenMobility returns the squares a queen can reach from sq on an empty board.
func SuperQueenMobility(sq Square) Bitboard {
	return bbSuperAttack[sq]
}

// KingMobility returns all squares a king can reach from sq.
// Doesn't include castling.
func KingMobility(sq Square) Bitboard {
	return bbKingAttack[sq]
}
