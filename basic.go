// Copyright 2014-2016 The Zurichess Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// basic.go defines basic types.
//
//go:generate stringer -type Figure
//go:generate stringer -type Color
//go:generate stringer -type Piece
//go:generate stringer -type MoveType

package board

import (
	"fmt"
	"math/bits"
)

var (
	// maps figures to symbols for move notations
	lanFigureToSymbol = [...]string{"", "", "N", "B", "R", "Q", "K"}
	uciFigureToSymbol = [...]string{"", "", "n", "b", "r", "q", "k"}
)

// Square identifies the location on the board.
type Square uint

const (
	// Set of possible board squares.

	SquareA1 = Square(iota)
	SquareB1
	SquareC1
	SquareD1
	SquareE1
	SquareF1
	SquareG1
	SquareH1
	SquareA2
	SquareB2
	SquareC2
	SquareD2
	SquareE2
	SquareF2
	SquareG2
	SquareH2
	SquareA3
	SquareB3
	SquareC3
	SquareD3
	SquareE3
	SquareF3
	SquareG3
	SquareH3
	SquareA4
	SquareB4
	SquareC4
	SquareD4
	SquareE4
	SquareF4
	SquareG4
	SquareH4
	SquareA5
	SquareB5
	SquareC5
	SquareD5
	SquareE5
	SquareF5
	SquareG5
	SquareH5
	SquareA6
	SquareB6
	SquareC6
	SquareD6
	SquareE6
	SquareF6
	SquareG6
	SquareH6
	SquareA7
	SquareB7
	SquareC7
	SquareD7
	SquareE7
	SquareF7
	SquareG7
	SquareH7
	SquareA8
	SquareB8
	SquareC8
	SquareD8
	SquareE8
	SquareF8
	SquareG8
	SquareH8

	SquareArraySize = int(iota)
	SquareMinValue  = SquareA1
	SquareMaxValue  = SquareH8
)

// RankFile returns a square with rank r and file f.
// r and f should be between 0 and 7.
func RankFile(r, f int) Square {
	return Square(r*8 + f)
}

// SquareFromString parses a square from a string.
// The string has standard chess format [a-h][1-8].
func SquareFromString(s string) (Square, error) {
	if len(s) != 2 {
		return SquareA1, fmt.Errorf("invalid square %s", s)
	}

	f, r := -1, -1
	if 'a' <= s[0] && s[0] <= 'h' {
		f = int(s[0] - 'a')
	}
	if 'A' <= s[0] && s[0] <= 'H' {
		f = int(s[0] - 'A')
	}
	if '1' <= s[1] && s[1] <= '8' {
		r = int(s[1] - '1')
	}
	if f == -1 || r == -1 {
		return SquareA1, fmt.Errorf("invalid square %s", s)
	}

	return RankFile(r, f), nil
}

// Bitboard returns a bitboard that has sq set.
func (sq Square) Bitboard() Bitboard {
	return 1 << uint(sq)
}

// Rank returns a number from 0 to 7 representing the rank of the square.
func (sq Square) Rank() int {
	return int(sq / 8)
}

// File returns a number from 0 to 7 representing the file of the square.
func (sq Square) File() int {
	return int(sq % 8)
}

// POV returns the square from col's point of view.
// That is for Black the rank is flipped, file stays the same.
// Useful in evaluation based on king's or pawns' positions.
func (sq Square) POV(col Color) Square {
	return sq ^ (Square(col-2) & 0x38)
}

func (sq Square) String() string {
	squareToString := "a1b1c1d1e1f1g1h1a2b2c2d2e2f2g2h2a3b3c3d3e3f3g3h3a4b4c4d4e4f4g4h4a5b5c5d5e5f5g5h5a6b6c6d6e6f6g6h6a7b7c7d7e7f7g7h7a8b8c8d8e8f8g8h8"
	return squareToString[sq*2 : sq*2+2]
}

// Figure represents a piece without a color.
type Figure uint

const (
	// Set of possible chess figures.

	NoFigure Figure = iota
	Pawn
	Knight
	Bishop
	Rook
	Queen
	King

	FigureArraySize = int(iota)
	FigureMinValue  = Pawn
	FigureMaxValue  = King
)

// Color represents a side.
type Color uint

const (
	// Set of possible chess colors.

	NoColor Color = iota
	Black
	White
	_

	ColorArraySize = int(iota)
	ColorMinValue  = Black
	ColorMaxValue  = White
)

// Opposite returns the reversed color.
// Result is undefined if c is not White or Black.
func (c Color) Opposite() Color {
	return White + Black - c
}

// Multiplier returns -1 for Black, 1 for White. Useful
// to compute the position score relative to current player.
// Result is undefined if c is not White or Black.
func (c Color) Multiplier() int32 {
	return int32(int(c)*2 - 3)
}

// Piece is a figure owned by one side.
type Piece uint

// Piece constants must stay in sync with ColorFigure
// The order of pieces must match Polyglot format:
// http://hgm.nubati.net/book_format.html
const (
	NoPiece Piece = iota
	_
	BlackPawn
	WhitePawn
	BlackKnight
	WhiteKnight
	BlackBishop
	WhiteBishop
	BlackRook
	WhiteRook
	BlackQueen
	WhiteQueen
	BlackKing
	WhiteKing
	_
	_

	PieceArraySize = int(iota)
	PieceMinValue  = BlackPawn
	PieceMaxValue  = WhiteKing
)

// ColorFigure returns a piece with col and fig.
func ColorFigure(col Color, fig Figure) Piece {
	return Piece(fig<<1) + Piece(col>>1)
}

// Color returns piece's color.
func (pi Piece) Color() Color {
	return Color(21844 >> pi & 3)
}

// Figure returns piece's figure.
func (pi Piece) Figure() Figure {
	return Figure(pi) >> 1
}

// Bitboard is a set representing the 8x8 chess board squares.
type Bitboard uint64

const (
	BbEmpty          Bitboard = 0x0000000000000000
	BbFull           Bitboard = 0xffffffffffffffff
	BbBorder         Bitboard = 0xff818181818181ff
	BbPawnStartRank  Bitboard = 0x00ff00000000ff00
	BbPawnDoubleRank Bitboard = 0x000000ffff000000
	BbBlackSquares   Bitboard = 0xaa55aa55aa55aa55
	BbWhiteSquares   Bitboard = 0x55aa55aa55aa55aa
)

const (
	BbFileA Bitboard = 0x101010101010101 << iota
	BbFileB
	BbFileC
	BbFileD
	BbFileE
	BbFileF
	BbFileG
	BbFileH
)

const (
	BbRank1 Bitboard = 0x0000000000000FF << (8 * iota)
	BbRank2
	BbRank3
	BbRank4
	BbRank5
	BbRank6
	BbRank7
	BbRank8
)

// RankBb returns a bitboard with all bits on rank set.
func RankBb(rank int) Bitboard {
	return BbRank1 << uint(8*rank)
}

// FileBb returns a bitboard with all bits on file set.
func FileBb(file int) Bitboard {
	return BbFileA << uint(file)
}

// North shifts all squares one rank up.
func North(bb Bitboard) Bitboard {
	return bb << 8
}

// South shifts all squares one rank down.
func South(bb Bitboard) Bitboard {
	return bb >> 8
}

// East shifts all squares one file right.
func East(bb Bitboard) Bitboard {
	return bb &^ BbFileH << 1
}

// West shifts all squares one file left.
func West(bb Bitboard) Bitboard {
	return bb &^ BbFileA >> 1
}

// Forward returns bb shifted one rank forward wrt color.
func Forward(col Color, bb Bitboard) Bitboard {
	if col == White {
		return bb << 8
	}
	if col == Black {
		return bb >> 8
	}
	return bb
}

// Backward returns bb shifted one rank backward wrt color.
func Backward(col Color, bb Bitboard) Bitboard {
	if col == White {
		return bb >> 8
	}
	if col == Black {
		return bb << 8
	}
	return bb
}

// Fill returns a bitboard with all files with squares filled.
func Fill(bb Bitboard) Bitboard {
	return NorthFill(bb) | SouthFill(bb)
}

// ForwardSpan computes forward span wrt color.
func ForwardSpan(col Color, bb Bitboard) Bitboard {
	if col == White {
		return NorthSpan(bb)
	}
	if col == Black {
		return SouthSpan(bb)
	}
	return bb
}

// ForwardFill computes forward fill wrt color.
func ForwardFill(col Color, bb Bitboard) Bitboard {
	if col == White {
		return NorthFill(bb)
	}
	if col == Black {
		return SouthFill(bb)
	}
	return bb
}

// BackwardSpan computes backward span wrt color.
func BackwardSpan(col Color, bb Bitboard) Bitboard {
	if col == White {
		return SouthSpan(bb)
	}
	if col == Black {
		return NorthSpan(bb)
	}
	return bb
}

// BackwardFill computes forward fill wrt color.
func BackwardFill(col Color, bb Bitboard) Bitboard {
	if col == White {
		return SouthFill(bb)
	}
	if col == Black {
		return NorthFill(bb)
	}
	return bb
}

// NorthFill returns a bitboard with all north bits set.
func NorthFill(bb Bitboard) Bitboard {
	bb |= (bb << 8)
	bb |= (bb << 16)
	bb |= (bb << 32)
	return bb
}

// NorthSpan is like NorthFill shifted on up.
func NorthSpan(bb Bitboard) Bitboard {
	return NorthFill(North(bb))
}

// SouthFill returns a bitboard with all south bits set.
func SouthFill(bb Bitboard) Bitboard {
	bb |= (bb >> 8)
	bb |= (bb >> 16)
	bb |= (bb >> 32)
	return bb
}

// SouthSpan is like SouthFill shifted on up.
func SouthSpan(bb Bitboard) Bitboard {
	return SouthFill(South(bb))
}

// Has returns bb if sq is occupied in bitboard.
func (bb Bitboard) Has(sq Square) bool {
	return bb>>sq&1 != 0
}

// AsSquare returns the occupied square if the bitboard has a single piece.
// If the board has more then one piece the result is undefined.
func (bb Bitboard) AsSquare() Square {
	return Square(bits.TrailingZeros64(uint64(bb)) & 0x3f)
}

// LSB picks a square in the board.
// Returns empty board for empty board.
func (bb Bitboard) LSB() Bitboard {
	return bb & (-bb)
}

// Count returns the number of squares set in bb.
func (bb Bitboard) Count() int32 {
	return int32(bits.OnesCount64(uint64(bb)))
}

// Pop pops a set square from the bitboard.
func (bb *Bitboard) Pop() Square {
	sq := *bb & (-*bb)
	*bb -= sq
	return Square(bits.TrailingZeros64(uint64(sq)) & 0x3f)
}

// MoveType defines the move type.
type MoveType uint8

const (
	NoMove    MoveType = iota // no move or null move
	Normal                    // regular move
	Promotion                 // pawn is promoted. Move.Promotion() gives the new piece
	Castling                  // king castles
	Enpassant                 // pawn takes enpassant
)

const (
	// NullMove is a move that does nothing. Has value to 0.
	NullMove = Move(0)
)

// Move stores a position dependent move.
//
// Bit representation
//   00.00.00.3f - from
//   00.00.3f.00 - to
//   00.0f.00.00 - move type
//   00.f0.00.00 - target
//   0f.00.00.00 - capture
//   f0.00.00.00 - piece
type Move uint32

// MakeMove constructs a move.
func MakeMove(moveType MoveType, from, to Square, capture, target Piece) Move {
	piece := target
	if moveType == Promotion {
		piece = ColorFigure(target.Color(), Pawn)
	}

	return Move(from)<<0 +
		Move(to)<<8 +
		Move(moveType)<<16 +
		Move(target)<<20 +
		Move(capture)<<24 +
		Move(piece)<<28
}

// From returns the starting square.
func (m Move) From() Square {
	return Square(m >> 0 & 0x3f)
}

// To returns the destination square.
func (m Move) To() Square {
	return Square(m >> 8 & 0x3f)
}

// MoveType returns the move type.
func (m Move) MoveType() MoveType {
	return MoveType(m >> 16 & 0xf)
}

// CaptureSquare returns the captured piece square.
// If no piece is captured, the result is the destination square.
func (m Move) CaptureSquare() Square {
	if m.MoveType() != Enpassant {
		return m.To()
	}
	return m.From()&0x38 + m.To()&0x7
}

// Capture returns the captured pieces.
func (m Move) Capture() Piece {
	return Piece(m >> 24 & 0xf)
}

// Target returns the piece on the to square after the move is executed.
func (m Move) Target() Piece {
	return Piece(m >> 20 & 0xf)
}

// Piece returns the piece moved.
func (m Move) Piece() Piece {
	return Piece(m >> 28 & 0xf)
}

// Color returns which player is moving.
func (m Move) Color() Color {
	return m.Piece().Color()
}

// Figure returns which figure is moved.
func (m Move) Figure() Figure {
	return m.Piece().Figure()
}

// Promotion returns the promoted piece if any.
func (m Move) Promotion() Piece {
	if m.MoveType() != Promotion {
		return NoPiece
	}
	return m.Target()
}

// IsViolent returns true if the move is a capture or a queen promotion.
// Castling and minor promotions (including captures) are not violent.
// TODO: IsViolent should be in sync with GenerateViolentMoves.
func (m Move) IsViolent() bool {
	if m.MoveType() != Promotion {
		return m.Capture() != NoPiece
	}
	return m.Target().Figure() == Queen
}

// IsQuiet returns true if the move is not violent.
func (m Move) IsQuiet() bool {
	return !m.IsViolent()
}

// UCI converts a move to UCI format.
// The protocol specification at http://wbec-ridderkerk.nl/html/UCIProtocol.html
// incorrectly states that this is the long algebraic notation (LAN).
func (m Move) UCI() string {
	return m.From().String() + m.To().String() + uciFigureToSymbol[m.Promotion().Figure()]
}

// LAN converts a move to Long Algebraic Notation.
// http://en.wikipedia.org/wiki/Algebraic_notation_%28chess%29#Long_algebraic_notation
// E.g. a2-a3, b7-b8Q, Nb1xc3, Ke1-c1 (white king queen side castling).
func (m Move) LAN() string {
	r := lanFigureToSymbol[m.Piece().Figure()] + m.From().String()
	if m.Capture() != NoPiece {
		r += "x"
	} else {
		r += "-"
	}
	r += m.To().String() + lanFigureToSymbol[m.Promotion().Figure()]
	return r
}

func (m Move) String() string {
	return m.LAN()
}

// Castle represents the castling rights mask.
type Castle uint

const (
	// WhiteOO indicates that White can castle on King side.
	WhiteOO Castle = 1 << iota
	// WhiteOOO indicates that White can castle on Queen side.
	WhiteOOO
	// BlackOO indicates that Black can castle on King side.
	BlackOO
	// BlackOOO indicates that Black can castle on Queen side.
	BlackOOO

	// NoCastle indicates no castling rights.
	NoCastle Castle = 0
	// AnyCastle indicates all castling rights.
	AnyCastle Castle = WhiteOO | WhiteOOO | BlackOO | BlackOOO

	CastleArraySize = int(AnyCastle + 1)
	CastleMinValue  = NoCastle
	CastleMaxValue  = AnyCastle
)

var castleToString = [...]string{
	"-", "K", "Q", "KQ", "k", "Kk", "Qk", "KQk", "q", "Kq", "Qq", "KQq", "kq", "Kkq", "Qkq", "KQkq",
}

func (c Castle) String() string {
	if c < NoCastle || c > AnyCastle {
		return fmt.Sprintf("Castle(%d)", c)
	}
	return castleToString[c]
}

// CastlingRook returns the rook moved during castling
// together with starting and stopping squares.
func CastlingRook(kingEnd Square) (Piece, Square, Square) {
	// Explanation how rookStart works for king on E1.
	// if kingEnd == C1 == b010, then rookStart == A1 == b000
	// if kingEnd == G1 == b110, then rookStart == H1 == b111
	// So bit 3 will set bit 2 and bit 1.
	//
	// Explanation how rookEnd works for king on E1.
	// if kingEnd == C1 == b010, then rookEnd == D1 == b011
	// if kingEnd == G1 == b110, then rookEnd == F1 == b101
	// So bit 3 will invert bit 2. bit 1 is always set.
	piece := Piece(Rook<<1) + (1 - Piece(kingEnd>>5))
	rookStart := kingEnd&^3 | (kingEnd & 4 >> 1) | (kingEnd & 4 >> 2)
	rookEnd := kingEnd ^ (kingEnd & 4 >> 1) | 1
	return piece, rookStart, rookEnd
}
