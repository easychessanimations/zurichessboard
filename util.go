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

// RammedPawns returns pawns on ranks 2, 3 for white
// and rank 6 and 7 blocking an advanced enemy pawn.
func RammedPawns(pos *Position, us Color) Bitboard {
	var bb Bitboard
	if us == White {
		bb = BbRank2 | BbRank3
	} else if us == Black {
		bb = BbRank7 | BbRank6
	}
	return pos.ByPiece(us, Pawn) & Backward(us, pos.ByPiece(us.Opposite(), Pawn)) & bb
}

// Minors returns a bitboard with all knights and bishops.
func Minors(pos *Position, us Color) Bitboard {
	return pos.ByPiece2(us, Knight, Bishop)
}

// Majors returns a bitboard with all rooks and queens.
func Majors(pos *Position, us Color) Bitboard {
	return pos.ByPiece2(us, Rook, Queen)
}

// MinorsAndMajors returns a bitboard with minor and major pieces.
func MinorsAndMajors(pos *Position, col Color) Bitboard {
	return pos.ByColor(col) &^ pos.ByFigure(Pawn) &^ pos.ByFigure(King)
}

// OpenFiles returns all fully set files with no pawns.
func OpenFiles(pos *Position, us Color) Bitboard {
	pawns := pos.ByFigure(Pawn)
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
	bb = East(bb) | bb | West(bb)
	bb = North(bb) | bb | South(bb)
	return bb
}

// PawnPromotionSquare returns the propotion square of a col pawn on sq.
// Undefined behaviour if col is not White or Black.
func PawnPromotionSquare(col Color, sq Square) Square {
	if col == White {
		return sq | 0x38
	}
	if col == Black {
		return sq &^ 0x38
	}
	return sq
}

var homeRank = [ColorArraySize]int{0, 7, 0}

// HomeRank returns the rank of the king at the begining of the game.
// Result is undefined if c is not White or Black.
func HomeRank(col Color) int {
	return homeRank[col]
}
