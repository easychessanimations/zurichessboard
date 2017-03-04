// Copyright 2014-2017 The Zurichess Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// util.go contains various utility functions.

package board

// PawnThreats returns the squares threatened by our pawns.
func PawnThreats(pos *Position, us Color) Bitboard {
	ours := pos.ByPiece(us, Pawn)
	return Forward(us, East(ours)|West(ours))
}

// BackwardPawns returns the our backward pawns.
// A backward pawn is a pawn that has no pawns behind them on its file or
// adjacent file, it's not isolated and cannot advance safely.
func BackwardPawns(pos *Position, us Color) Bitboard {
	ours := pos.ByPiece(us, Pawn)
	behind := ForwardFill(us, East(ours)|West(ours))
	doubled := BackwardSpan(us, ours)
	isolated := IsolatedPawns(pos, us)
	return ours & Backward(us, PawnThreats(pos, us.Opposite())) &^ behind &^ doubled &^ isolated
}

// DoubledPawns returns a bitboard with all doubled pawns.
func DoubledPawns(pos *Position, us Color) Bitboard {
	ours := pos.ByPiece(us, Pawn)
	return ours & Backward(us, ours)
}

// IsolatedPawns returns a bitboard with all isolated pawns.
func IsolatedPawns(pos *Position, us Color) Bitboard {
	ours := pos.ByPiece(us, Pawn)
	wings := East(ours) | West(ours)
	return ours &^ Fill(wings)
}

// PassedPawns returns a bitboard with all passed pawns.
func PassedPawns(pos *Position, us Color) Bitboard {
	// From white's POV: w - white pawn, b - black pawn, x - non-passed pawns.
	// ........
	// .....w..
	// .....x..
	// ..b..x..
	// .xxx.x..
	// .xxx.x..
	ours := pos.ByPiece(us, Pawn)
	theirs := pos.ByPiece(us.Opposite(), Pawn)
	theirs |= East(theirs) | West(theirs)
	block := BackwardSpan(us, theirs|ours)
	return ours &^ block
}

// ConnectedPawns returns a bitboad with all connected pawns.
func ConnectedPawns(pos *Position, us Color) Bitboard {
	ours := pos.ByPiece(us, Pawn)
	wings := East(ours) | West(ours)
	return ours & (North(wings) | wings | South(wings))
}

// Minors returns a bitboard with all knights and bishops.
func Minors(pos *Position, us Color) Bitboard {
	return pos.ByPiece2(us, Knight, Bishop)
}

// Majors returns a bitboard with all rooks and queens.
func Majors(pos *Position, us Color) Bitboard {
	return pos.ByPiece2(us, Rook, Queen)
}

// OpenFiles returns all fully set files with no pawns.
func OpenFiles(pos *Position, us Color) Bitboard {
	pawns := pos.ByFigure[Pawn]
	return ^Fill(pawns)
}

// SemiOpenFiles returns all fully set files with enemy pawns, but no friendly pawns.
func SemiOpenFiles(pos *Position, us Color) Bitboard {
	ours := pos.ByPiece(us, Pawn)
	theirs := pos.ByPiece(us.Opposite(), Pawn)
	return Fill(theirs) &^ Fill(ours)
}

// KingArea returns an area around king.
func KingArea(pos *Position, us Color) Bitboard {
        bb := pos.ByPiece(us, King)
        bb = East(bb)|bb|West(bb)
        bb = North(bb)|bb|South(bb)
        return bb
}
