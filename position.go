// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package board

import (
	"fmt"
	"log"
	"math/rand"
	"strconv"
)

const (
	// Violent indicates captures (inluding en passant) and queen promotions.
	Violent int = 1 << iota
	// Quiet are all other moves including minor promotions and castling.
	Quiet
	// All moves (deprecated).
	All = Violent | Quiet
)

var (
	// FENStartPos is the FEN string of the starting position.
	FENStartPos = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

	// Which castle rights are lost when pieces are moved.
	lostCastleRights [64]Castle

	// The zobrist* arrays contain magic numbers used for Zobrist hashing.
	// More information on Zobrist hashing can be found in the paper:
	// http://research.cs.wisc.edu/techreports/1970/TR88.pdf
	// For correctness testing and the posibility to add
	// book handling code later zurichess uses Polyglot hashes
	// and key computing algorithm.
	zobristPiece     [PieceArraySize][SquareArraySize]uint64
	zobristEnpassant [SquareArraySize]uint64
	zobristCastle    [CastleArraySize]uint64
	zobristColor     [ColorArraySize]uint64

	// Maps runes to figures.
	symbolToFigure = map[rune]Figure{
		'p': Pawn,
		'n': Knight,
		'b': Bishop,
		'r': Rook,
		'q': Queen,
		'k': King,

		'P': Pawn,
		'N': Knight,
		'B': Bishop,
		'R': Rook,
		'Q': Queen,
		'K': King,
	}
)

type state struct {
	Zobrist         uint64 // Zobrist key, can be zero
	Move            Move   // last move played.
	HalfmoveClock   int    // last ply when a pawn was moved or a capture was made.
	EnpassantSquare Square // en passant square. If no e.p, then SquareA1.
	CastlingAbility Castle // remaining castling rights.
}

// Position represents the chess board and keeps track of the move history.
type Position struct {
	ByFigure   [FigureArraySize]Bitboard // bitboards of square occupancy by figure.
	ByColor    [ColorArraySize]Bitboard  // bitboards of square occupancy by color.
	sideToMove Color                     // which side is to move. sideToMove is updated by DoMove and UndoMove.
	Ply        int                       // current ply
	NumPieces  [PieceArraySize]int32     // number of pieces of each kind

	fullmoveCounter int     // fullmove counter, incremented after black move
	states          []state // a state for each Ply
	curr            *state  // current state
}

// NewPosition returns a new position representing an empty board.
func NewPosition() *Position {
	pos := &Position{
		fullmoveCounter: 1,
		states:          make([]state, 1, 4),
	}
	pos.curr = &pos.states[pos.Ply]
	return pos
}

// String returns position in FEN format.
// For table format use PrettyPrint.
func (pos *Position) String() string {
	s := FormatPiecePlacement(pos)
	s += " " + FormatSideToMove(pos)
	s += " " + FormatCastlingAbility(pos)
	s += " " + FormatEnpassantSquare(pos)
	s += " " + strconv.Itoa(pos.curr.HalfmoveClock)
	s += " " + strconv.Itoa(pos.fullmoveCounter)
	return s
}

// prev returns state at previous ply.
func (pos *Position) prev() *state {
	return &pos.states[len(pos.states)-1]
}

// popState pops one ply.
func (pos *Position) popState() {
	len := len(pos.states) - 1
	pos.states = pos.states[:len]
	pos.curr = &pos.states[len-1]
	pos.Ply--
}

// pushState adds one ply.
func (pos *Position) pushState() {
	len := len(pos.states)
	pos.states = append(pos.states, pos.states[len-1])
	pos.curr = &pos.states[len]
	pos.Ply++
}

// FullmoveCounter returns the number of full moves. Starts at 1.
func (pos *Position) FullmoveCounter() int {
	return pos.fullmoveCounter
}

// SetFullmoveCounter sets the number of full moves.
func (pos *Position) SetFullmoveCounter(n int) {
	pos.fullmoveCounter = n
}

// HalfmoveClock returns the number of halfmoves since the last capture or pawn advance.
func (pos *Position) HalfmoveClock() int {
	return pos.curr.HalfmoveClock
}

// SetHalfmoveClock sets the number of halfmoves since the last capture or pawn advance.
func (pos *Position) SetHalfmoveClock(n int) {
	pos.curr.HalfmoveClock = n
}

// Us returns the current player to move.
//
// Us/Them is based on Glaurung terminology.
func (pos *Position) Us() Color {
	return pos.sideToMove
}

// Them returns the player awaiting to move.
func (pos *Position) Them() Color {
	return pos.sideToMove.Opposite()
}

// IsEnpassantSquare returns true if sq is the en passant square.
func (pos *Position) IsEnpassantSquare(sq Square) bool {
	return sq != SquareA1 && sq == pos.EnpassantSquare()
}

// EnpassantSquare returns the en passant square.
// If none, return SquareA1.
// This uses the polyglot definition: if the en. passant square is
// not attacked by the enemy, then EnpassantSquare() returns SquareA1.
func (pos *Position) EnpassantSquare() Square {
	return pos.curr.EnpassantSquare
}

// CastlingAbility returns kings' castling ability.
func (pos *Position) CastlingAbility() Castle {
	return pos.curr.CastlingAbility
}

// LastMove returns the last move played, if any.
func (pos *Position) LastMove() Move {
	return pos.curr.Move
}

// Zobrist returns the zobrist key of the position. Never returns 0.
func (pos *Position) Zobrist() uint64 {
	if pos.curr.Zobrist != 0 {
		return pos.curr.Zobrist
	}
	return 0x4204fa763da3abeb
}

// MinorsAndMajors returns a bitboard with minor and major pieces.
func (pos *Position) MinorsAndMajors(col Color) Bitboard {
	return pos.ByColor[col] &^ pos.ByFigure[Pawn] &^ pos.ByFigure[King]
}

// IsPseudoLegal returns true if m is a pseudo legal move for pos.
// It returns true iff m can be executed even if own king is in check
// after the move. NullMove is not a valid move.
// Assumes that there exists a position for which this move is valid,
// e.g. not a rook moving diagonally or a pawn promoting on 4th rank.
func (pos *Position) IsPseudoLegal(m Move) bool {
	if m == NullMove ||
		m.Color() != pos.Us() ||
		!pos.Has(m.From(), m.Piece()) ||
		!pos.Has(m.CaptureSquare(), m.Capture()) {
		return false
	}

	from, to := m.From(), m.To()
	all := pos.ByColor[White] | pos.ByColor[Black]

	switch m.Figure() {
	case Pawn:
		// Pawn move is tested above. Promotion is always correct.
		if m.MoveType() == Enpassant && !pos.IsEnpassantSquare(m.To()) {
			return false
		}
		if BbPawnStartRank.Has(m.From()) && BbPawnDoubleRank.Has(m.To()) && !pos.IsEmpty((m.From()+m.To())/2) {
			return false
		}
		return true
	case Knight: // Knight jumps around.
		return true
	case Bishop, Rook, Queen:
		// bbSuperAttack contains queen's mobility on an empty board.
		// Intersecting mobility from `from` and from `to` we get
		// the diagonal, rank or file on which the piece moved. If the
		// intersection is empty we are sure that no other piece was in the way.
		if bbSuperAttack[from]&bbSuperAttack[to]&all == BbEmpty {
			return true
		}
		switch m.Figure() {
		case Bishop:
			return BishopMobility(from, all).Has(to)
		case Rook:
			return RookMobility(from, all).Has(to)
		case Queen:
			return QueenMobility(from, all).Has(to)
		}
	case King:
		if m.MoveType() == Normal {
			return bbKingAttack[from].Has(to)
		}

		// m.MoveType() == Castling
		if m.Color() == White && m.To() == SquareG1 {
			if pos.CastlingAbility()&WhiteOO == 0 ||
				!pos.IsEmpty(SquareF1) || !pos.IsEmpty(SquareG1) {
				return false
			}
		}
		if m.Color() == White && m.To() == SquareC1 {
			if pos.CastlingAbility()&WhiteOOO == 0 ||
				!pos.IsEmpty(SquareB1) ||
				!pos.IsEmpty(SquareC1) ||
				!pos.IsEmpty(SquareD1) {
				return false
			}
		}
		if m.Color() == Black && m.To() == SquareG8 {
			if pos.CastlingAbility()&BlackOO == 0 ||
				!pos.IsEmpty(SquareF8) ||
				!pos.IsEmpty(SquareG8) {
				return false
			}
		}
		if m.Color() == Black && m.To() == SquareC8 {
			if pos.CastlingAbility()&BlackOOO == 0 ||
				!pos.IsEmpty(SquareB8) ||
				!pos.IsEmpty(SquareC8) ||
				!pos.IsEmpty(SquareD8) {
				return false
			}
		}
		rook, start, end := CastlingRook(m.To())
		if pos.Get(start) != rook {
			return false
		}
		them := m.Color().Opposite()
		if pos.GetAttacker(m.From(), them) != NoFigure ||
			pos.GetAttacker(end, them) != NoFigure ||
			pos.GetAttacker(m.To(), them) != NoFigure {
			return false
		}
	default:
		panic("unreachable")
	}

	return true
}

// Verify check the validity of the position.
// Mostly used for debugging purposes.
func (pos *Position) Verify() error {
	if bb := pos.ByColor[White] & pos.ByColor[Black]; bb != 0 {
		sq := bb.Pop()
		return fmt.Errorf("Square %v is both White and Black", sq)
	}
	// Check that there is at most one king.
	// Catches castling issues.
	for col := ColorMinValue; col <= ColorMaxValue; col++ {
		bb := pos.ByPiece(col, King)
		sq := bb.Pop()
		if bb != 0 {
			sq2 := bb.Pop()
			return fmt.Errorf("More than one King for %v at %v and %v", col, sq, sq2)
		}
	}

	// Verifies that pieces have the right color.
	for col := ColorMinValue; col <= ColorMaxValue; col++ {
		for bb := pos.ByColor[col]; bb != 0; {
			sq := bb.Pop()
			pi := pos.Get(sq)
			if pi.Color() != col {
				return fmt.Errorf("Expected color %v, got %v", col, pi)
			}
		}
	}

	// Verifies that no two pieces sit on the same cell.
	for pi1 := PieceMinValue; pi1 <= PieceMaxValue; pi1++ {
		for pi2 := pi1 + 1; pi2 <= PieceMaxValue; pi2++ {
			if pos.ByPiece(pi1.Color(), pi1.Figure())&pos.ByPiece(pi2.Color(), pi2.Figure()) != 0 {
				return fmt.Errorf("%v and %v overlap", pi1, pi2)
			}
		}
	}

	// Verifies that en passant square is empty.
	if sq := pos.curr.EnpassantSquare; sq != SquareA1 && !pos.IsEmpty(sq) {
		return fmt.Errorf("Expected empty en passant square %v, got %v", sq, pos.Get(sq))
	}

	return nil
}

// SetCastlingAbility sets the side to move, correctly updating the Zobrist key.
func (pos *Position) SetCastlingAbility(castle Castle) {
	if pos.curr.CastlingAbility == castle {
		return
	}

	pos.curr.Zobrist ^= zobristCastle[pos.curr.CastlingAbility]
	pos.curr.CastlingAbility = castle
	pos.curr.Zobrist ^= zobristCastle[pos.curr.CastlingAbility]
}

// SetSideToMove sets the side to move, correctly updating the Zobrist key.
func (pos *Position) SetSideToMove(col Color) {
	pos.curr.Zobrist ^= zobristColor[pos.sideToMove]
	pos.sideToMove = col
	pos.curr.Zobrist ^= zobristColor[pos.sideToMove]
}

// SetEnpassantSquare sets the en passant square correctly updating the Zobrist key.
func (pos *Position) SetEnpassantSquare(epsq Square) {
	if epsq != SquareA1 {
		// In polyglot the hash key for en passant is updated only if
		// an en passant capture is possible next move. In other words
		// if there is an enemy pawn next to the end square of the move.
		var theirs Bitboard
		var sq Square
		if epsq.Rank() == 2 { // White
			theirs, sq = pos.ByPiece(Black, Pawn), RankFile(3, epsq.File())
		} else if epsq.Rank() == 5 { // Black
			theirs, sq = pos.ByPiece(White, Pawn), RankFile(4, epsq.File())
		} else {
			panic("bad en passant square")
		}

		if (sq.File() == 0 || !theirs.Has(sq-1)) && (sq.File() == 7 || !theirs.Has(sq+1)) {
			epsq = SquareA1
		}
	}

	pos.curr.Zobrist ^= zobristEnpassant[pos.curr.EnpassantSquare]
	pos.curr.EnpassantSquare = epsq
	pos.curr.Zobrist ^= zobristEnpassant[pos.curr.EnpassantSquare]
}

// ByPiece is a shortcut for ByColor[col]&ByFigure[fig].
func (pos *Position) ByPiece(col Color, fig Figure) Bitboard {
	return pos.ByColor[col] & pos.ByFigure[fig]
}

// ByPiece2 is a shortcut for ByColor[col]&(ByFigure[fig0]|ByFigure[fig1])
func (pos *Position) ByPiece2(col Color, fig0, fig1 Figure) Bitboard {
	return pos.ByColor[col] & (pos.ByFigure[fig0] | pos.ByFigure[fig1])
}

// Put puts a piece on the board.
// Does nothing if pi is NoPiece. Does not validate input.
func (pos *Position) Put(sq Square, pi Piece) {
	if pi != NoPiece {
		pos.curr.Zobrist ^= zobristPiece[pi][sq]
		bb := sq.Bitboard()
		pos.ByColor[pi.Color()] |= bb
		pos.ByFigure[pi.Figure()] |= bb
		pos.NumPieces[pi]++
	}
}

// Remove removes a piece from the table.
// Does nothing if pi is NoPiece. Does not validate input.
func (pos *Position) Remove(sq Square, pi Piece) {
	if pi != NoPiece {
		pos.curr.Zobrist ^= zobristPiece[pi][sq]
		bb := ^sq.Bitboard()
		pos.ByColor[pi.Color()] &= bb
		pos.ByFigure[pi.Figure()] &= bb
		pos.NumPieces[pi]--
	}
}

// IsEmpty returns true if there is no piece at sq.
func (pos *Position) IsEmpty(sq Square) bool {
	return !(pos.ByColor[White] | pos.ByColor[Black]).Has(sq)
}

// Has returns true if pi is in sq.
// Equivalent to Get(sq) == pi, but faster.
func (pos *Position) Has(sq Square, pi Piece) bool {
	if pi != NoPiece {
		return pos.ByColor[pi.Color()].Has(sq) && pos.ByFigure[pi.Figure()].Has(sq)
	}
	return pos.IsEmpty(sq)
}

// Get returns the piece at sq.
func (pos *Position) Get(sq Square) Piece {
	var col Color
	if pos.ByColor[White].Has(sq) {
		col = White
	} else if pos.ByColor[Black].Has(sq) {
		col = Black
	} else {
		return NoPiece
	}

	for fig := FigureMinValue; fig <= FigureMaxValue; fig++ {
		if pos.ByFigure[fig].Has(sq) {
			return ColorFigure(col, fig)
		}
	}
	panic("unreachable: square has color, but no figure")
}

// HasLegalMoves returns true if current side has any legal moves.
// This function is very expensive.
func (pos *Position) HasLegalMoves() bool {
	var moves []Move
	pos.GenerateMoves(Violent|Quiet, &moves)
	for _, m := range moves {
		pos.DoMove(m)
		checked := pos.IsChecked(pos.Them())
		pos.UndoMove()
		if !checked { // check if move didn't leave the player in check
			return true
		}
	}
	return false
}

// InsufficientMaterial returns true if the position is theoretical draw.
func (pos *Position) InsufficientMaterial() bool {
	// K vs K is draw.
	noKings := (pos.ByColor[White] | pos.ByColor[Black]) &^ pos.ByFigure[King]
	if noKings == 0 {
		return true
	}
	// KN vs K is theoretical draw.
	if noKings == pos.ByFigure[Knight] && pos.ByFigure[Knight].CountMax2() == 1 {
		return true
	}
	// KB* vs KB* is theoretical draw if all bishops are on the same square color.
	if bishops := pos.ByFigure[Bishop]; noKings == bishops {
		if bishops&BbWhiteSquares == bishops || bishops&BbBlackSquares == bishops {
			return true
		}
	}
	return false
}

// ThreeFoldRepetition returns whether current position was seen three times already.
// Returns minimum between 3 and the actual number of repetitions.
func (pos *Position) ThreeFoldRepetition() int {
	c, z := 0, pos.Zobrist()
	for i := 0; i < len(pos.states) && i <= pos.curr.HalfmoveClock; i += 2 {
		j := len(pos.states) - 1 - i
		if j != 0 && pos.states[j].Move == NullMove {
			// Search uses NullMove for Null Move Pruning heuristic.  A couple of consecutive
			// NullMoves results in a repeated position, but it's not actually a repeat.
			break
		}
		if pos.states[j].Zobrist == z {
			if c++; c == 3 {
				break
			}
		}
	}
	return c
}

// FiftyMoveRule returns true if 50 moves (on each side) were made
// without any capture of pawn move.
//
// If FiftyMoveRule returns true, the position is a draw.
func (pos *Position) FiftyMoveRule() bool {
	return pos.curr.HalfmoveClock >= 100
}

// IsChecked returns true if side's king is checked.
func (pos *Position) IsChecked(side Color) bool {
	kingSq := pos.ByPiece(side, King).AsSquare()
	return pos.GetAttacker(kingSq, side.Opposite()) != NoFigure
}

// GivesCheck returns true if the opposite side is in check after m is executed.
func (pos *Position) GivesCheck(m Move) bool {
	if m.MoveType() == Castling {
		// TODO: Bail out on castling because it can check via rook and king.
		pos.DoMove(m)
		givesCheck := pos.IsChecked(pos.Us())
		pos.UndoMove()
		return givesCheck
	}

	us := pos.Us()
	all := pos.ByColor[White] | pos.ByColor[Black]
	all = all&^m.From().Bitboard()&^m.CaptureSquare().Bitboard() | m.To().Bitboard()
	kingSq := pos.ByPiece(pos.Them(), King).AsSquare()
	fig := m.Target().Figure()

	// Test pawn and king.
	if fig == Pawn {
		bb := m.To().Bitboard()
		bb = Forward(us, bb)
		bb = East(bb) | West(bb)
		if bb.Has(kingSq) {
			return true
		}
	}
	mob := KnightMobility(kingSq) &^ m.From().Bitboard()
	if mob&pos.ByPiece(us, Knight) != 0 ||
		mob.Has(m.To()) && fig == Knight {
		return true
	}

	// Fast check whether king can be attacked by a Bishop, Rook, Queen, King
	// using the moves of a Queen on an empty table.
	ours := pos.ByColor[pos.Us()]&^m.From().Bitboard() | m.To().Bitboard()
	if ours&bbSuperAttack[kingSq] == 0 {
		return false
	}

	// Test bishop, rook, queen and king.
	mob = BishopMobility(kingSq, all) &^ m.From().Bitboard()
	if mob&pos.ByPiece2(us, Bishop, Queen) != 0 ||
		mob.Has(m.To()) && (fig == Bishop || fig == Queen) {
		return true
	}
	mob = RookMobility(kingSq, all) &^ m.From().Bitboard()
	if mob&pos.ByPiece2(us, Rook, Queen) != 0 ||
		mob.Has(m.To()) && (fig == Rook || fig == Queen) {
		return true
	}
	// King checking another king is an illegal move,
	// but make the result consistent with IsChecked.
	mob = KingMobility(kingSq) &^ m.From().Bitboard()
	if mob&pos.ByPiece(us, King) != 0 ||
		mob.Has(m.To()) && fig == King {
		return true
	}

	return false
}

var prettyPieceToSymbol = []string{".", "?", "♟", "♙", "♞", "♘", "♝", "♗", "♜", "♖", "♛", "♕", "♚", "♔"}

// PrettyPrint pretty prints the current position to log.
func (pos *Position) PrettyPrint() {
	log.Println("zobrist =", pos.Zobrist())
	log.Println("fen =", pos.String())
	for r := 7; r >= 0; r-- {
		line := ""
		for f := 0; f < 8; f++ {
			sq := RankFile(r, f)
			if pos.IsEnpassantSquare(sq) {
				line += ","
			} else {
				line += prettyPieceToSymbol[pos.Get(sq)]
			}
		}
		if r == 7 && pos.Us() == Black {
			line += " *"
		}
		if r == 0 && pos.Us() == White {
			line += " *"
		}
		log.Println(line)
	}

}

// UCIToMove parses a move given in UCI format.
// s can be "a2a4" or "h7h8Q" for pawn promotion.
func (pos *Position) UCIToMove(s string) (Move, error) {
	if len(s) < 4 {
		return NullMove, fmt.Errorf("%s is too short", s)
	}

	from, err := SquareFromString(s[0:2])
	if err != nil {
		return NullMove, err
	}
	to, err := SquareFromString(s[2:4])
	if err != nil {
		return NullMove, err
	}

	moveType := Normal
	capt := pos.Get(to)
	target := pos.Get(from)

	pi := pos.Get(from)
	if pi.Figure() == Pawn && pos.IsEnpassantSquare(to) {
		moveType = Enpassant
		capt = ColorFigure(pos.Them(), Pawn)
	}
	if pi == WhiteKing && from == SquareE1 && (to == SquareC1 || to == SquareG1) {
		moveType = Castling
	}
	if pi == BlackKing && from == SquareE8 && (to == SquareC8 || to == SquareG8) {
		moveType = Castling
	}
	if pi.Figure() == Pawn && (to.Rank() == 0 || to.Rank() == 7) {
		if len(s) != 5 {
			return NullMove, fmt.Errorf("%s doesn't have a promotion piece", s)
		}
		moveType = Promotion
		target = ColorFigure(pos.Us(), symbolToFigure[rune(s[4])])
	} else {
		if len(s) != 4 {
			return NullMove, fmt.Errorf("%s move is too long", s)
		}
	}

	move := MakeMove(moveType, from, to, capt, target)
	if !pos.IsPseudoLegal(move) {
		return NullMove, fmt.Errorf("%s is not a valid move", s)
	}
	return move, nil
}

// DoMove executes a legal move.
func (pos *Position) DoMove(move Move) {
	pos.pushState()
	curr := pos.curr
	curr.Move = move

	// Update castling rights.
	pi := move.Piece()
	if pi != NoPiece { // nullmove cannot change castling ability
		pos.SetCastlingAbility(curr.CastlingAbility &^ lostCastleRights[move.From()] &^ lostCastleRights[move.To()])
	}
	// update fullmove counter.
	if pos.Us() == Black {
		pos.fullmoveCounter++
	}
	// Update halfmove clock.
	curr.HalfmoveClock++
	if pi.Figure() == Pawn || move.Capture() != NoPiece {
		curr.HalfmoveClock = 0
	}
	// Set Enpassant square for capturing.
	if pi.Figure() == Pawn && move.From().Rank()^move.To().Rank() == 2 {
		pos.SetEnpassantSquare((move.From() + move.To()) / 2)
	} else if pos.EnpassantSquare() != SquareA1 {
		pos.SetEnpassantSquare(SquareA1)
	}
	// Move rook on castling.
	if move.MoveType() == Castling {
		rook, start, end := CastlingRook(move.To())
		pos.Remove(start, rook)
		pos.Put(end, rook)
	}

	// Update the pieces on the chess board.
	pos.Remove(move.From(), pi)
	pos.Remove(move.CaptureSquare(), move.Capture())
	pos.Put(move.To(), move.Target())
	pos.SetSideToMove(pos.Them())
}

// UndoMove takes back the last move.
func (pos *Position) UndoMove() {
	move := pos.LastMove()
	pos.SetSideToMove(pos.Them())
	// CastlingAbility and EnpassantSquare are restored by pos.popState().
	// pos.SetCastlingAbility(pos.prev().CastlingAbility)
	// pos.SetEnpassantSquare(pos.prev().EnpassantSquare[1])

	// Modify the chess board.
	pi := move.Piece()
	pos.Put(move.From(), pi)
	pos.Remove(move.To(), move.Target())
	pos.Put(move.CaptureSquare(), move.Capture())

	// Move rook on castling.
	if move.MoveType() == Castling {
		rook, start, end := CastlingRook(move.To())
		pos.Put(start, rook)
		pos.Remove(end, rook)
	}

	if pos.Us() == Black {
		pos.fullmoveCounter--
	}

	pos.popState()
}

func (pos *Position) genPawnPromotions(kind int, moves *[]Move) {
	// Minimum and maximum promotion pieces.
	// Quiet -> Knight - Rook
	// Violent -> Queen
	pMin, pMax := Queen, Rook
	if kind&Violent != 0 {
		pMax = Queen
	}
	if kind&Quiet != 0 {
		pMin = Knight
	}

	// Get the pawns that can be promoted.
	us, them := pos.Us(), pos.Them()
	all := pos.ByColor[White] | pos.ByColor[Black]
	ours := pos.ByPiece(us, Pawn)
	theirs := pos.ByColor[them] // their pieces

	forward := Square(0)
	if us == White {
		ours &= BbRank7
		forward = RankFile(+1, 0)
	} else {
		ours &= BbRank2
		forward = RankFile(-1, 0)
	}

	for ours != 0 {
		from := ours.Pop()
		to := from + forward

		if !all.Has(to) { // advance front
			for p := pMin; p <= pMax; p++ {
				*moves = append(*moves, MakeMove(Promotion, from, to, NoPiece, ColorFigure(us, p)))
			}
		}
		if to.File() != 0 && theirs.Has(to-1) { // take west
			capt := pos.Get(to - 1)
			for p := pMin; p <= pMax; p++ {
				*moves = append(*moves, MakeMove(Promotion, from, to-1, capt, ColorFigure(us, p)))
			}
		}
		if to.File() != 7 && theirs.Has(to+1) { // take east
			capt := pos.Get(to + 1)
			for p := pMin; p <= pMax; p++ {
				*moves = append(*moves, MakeMove(Promotion, from, to+1, capt, ColorFigure(us, p)))
			}
		}
	}
}

// genPawnAdvanceMoves moves pawns one square.
// Does not generate promotions.
func (pos *Position) genPawnAdvanceMoves(kind int, moves *[]Move) {
	if kind&Quiet == 0 {
		return
	}

	ours := pos.ByPiece(pos.Us(), Pawn)
	occu := pos.ByColor[White] | pos.ByColor[Black]
	pawn := ColorFigure(pos.Us(), Pawn)

	var forward Square
	if pos.Us() == White {
		ours = ours &^ South(occu) &^ BbRank7
		forward = RankFile(+1, 0)
	} else {
		ours = ours &^ North(occu) &^ BbRank2
		forward = RankFile(-1, 0)
	}

	for ours != 0 {
		from := ours.Pop()
		to := from + forward
		*moves = append(*moves, MakeMove(Normal, from, to, NoPiece, pawn))
	}
}

// genPawnDoubleAdvanceMoves moves pawns two square.
func (pos *Position) genPawnDoubleAdvanceMoves(kind int, moves *[]Move) {
	if kind&Quiet == 0 {
		return
	}

	ours := pos.ByPiece(pos.Us(), Pawn)
	occu := pos.ByColor[White] | pos.ByColor[Black]
	pawn := ColorFigure(pos.Us(), Pawn)

	var forward Square
	if pos.Us() == White {
		ours &= RankBb(1) &^ South(occu) &^ South(South(occu))
		forward = RankFile(+2, 0)
	} else {
		ours &= RankBb(6) &^ North(occu) &^ North(North(occu))
		forward = RankFile(-2, 0)
	}

	for ours != 0 {
		from := ours.Pop()
		to := from + forward
		*moves = append(*moves, MakeMove(Normal, from, to, NoPiece, pawn))
	}
}

func (pos *Position) pawnCapture(to Square) (MoveType, Piece) {
	if pos.IsEnpassantSquare(to) {
		return Enpassant, ColorFigure(pos.Them(), Pawn)
	}
	return Normal, pos.Get(to)
}

// Generate pawn attacks moves.
// Does not generate promotions.
func (pos *Position) genPawnAttackMoves(kind int, moves *[]Move) {
	if kind&Violent == 0 {
		return
	}

	theirs := pos.ByColor[pos.Them()]
	if pos.curr.EnpassantSquare != SquareA1 {
		theirs |= pos.curr.EnpassantSquare.Bitboard()
	}

	forward := 0
	pawn := ColorFigure(pos.Us(), Pawn)
	ours := pos.ByPiece(pos.Us(), Pawn)
	if pos.Us() == White {
		ours = ours &^ BbRank7
		theirs = South(theirs)
		forward = +1
	} else {
		ours = ours &^ BbRank2
		theirs = North(theirs)
		forward = -1
	}

	// Left
	att := RankFile(forward, -1)
	for bbl := ours & East(theirs); bbl > 0; {
		from := bbl.Pop()
		to := from + att
		mt, capt := pos.pawnCapture(to)
		*moves = append(*moves, MakeMove(mt, from, to, capt, pawn))
	}

	// Right
	att = RankFile(forward, +1)
	for bbr := ours & West(theirs); bbr > 0; {
		from := bbr.Pop()
		to := from + att
		mt, capt := pos.pawnCapture(to)
		*moves = append(*moves, MakeMove(mt, from, to, capt, pawn))
	}
}

func (pos *Position) genBitboardMoves(pi Piece, from Square, att Bitboard, moves *[]Move) {
	for att != 0 {
		to := att.Pop()
		*moves = append(*moves, MakeMove(Normal, from, to, pos.Get(to), pi))
	}
}

func (pos *Position) getMask(kind int) Bitboard {
	mask := Bitboard(0)
	if kind&Violent != 0 {
		// Generate all attacks. Promotions are handled specially.
		mask |= pos.ByColor[pos.Them()]
	}
	if kind&Quiet != 0 {
		// Generate all non-attacks.
		mask |= ^(pos.ByColor[White] | pos.ByColor[Black])
	}
	// Minor promotions and castling are handled specially.
	return mask
}

func (pos *Position) genKnightMoves(mask Bitboard, moves *[]Move) {
	pi := ColorFigure(pos.Us(), Knight)
	for bb := pos.ByPiece(pos.Us(), Knight); bb != 0; {
		from := bb.Pop()
		att := bbKnightAttack[from] & mask
		pos.genBitboardMoves(pi, from, att, moves)
	}
}

func (pos *Position) genBishopMoves(fig Figure, mask Bitboard, moves *[]Move) {
	pi := ColorFigure(pos.Us(), fig)
	ref := pos.ByColor[White] | pos.ByColor[Black]
	for bb := pos.ByPiece(pos.Us(), fig); bb != 0; {
		from := bb.Pop()
		att := bishopMagic[from].Attack(ref) & mask
		pos.genBitboardMoves(pi, from, att, moves)
	}
}

func (pos *Position) genRookMoves(fig Figure, mask Bitboard, moves *[]Move) {
	pi := ColorFigure(pos.Us(), fig)
	ref := pos.ByColor[White] | pos.ByColor[Black]
	for bb := pos.ByPiece(pos.Us(), fig); bb != 0; {
		from := bb.Pop()
		att := rookMagic[from].Attack(ref) & mask
		pos.genBitboardMoves(pi, from, att, moves)
	}
}

func (pos *Position) genKingMovesNear(mask Bitboard, moves *[]Move) {
	pi := ColorFigure(pos.Us(), King)
	from := pos.ByPiece(pos.Us(), King).AsSquare()
	att := bbKingAttack[from] & mask
	pos.genBitboardMoves(pi, from, att, moves)
}

func (pos *Position) genKingCastles(kind int, moves *[]Move) {
	if kind&Quiet == 0 {
		return
	}

	rank := pos.Us().KingHomeRank()
	oo, ooo := WhiteOO, WhiteOOO
	if pos.Us() == Black {
		oo, ooo = BlackOO, BlackOOO
	}

	// Castle king side.
	if pos.curr.CastlingAbility&oo != 0 {
		r5 := RankFile(rank, 5)
		r6 := RankFile(rank, 6)
		if !pos.IsEmpty(r5) || !pos.IsEmpty(r6) {
			goto EndCastleOO
		}

		r4 := RankFile(rank, 4)
		if pos.GetAttacker(r4, pos.Them()) != NoFigure ||
			pos.GetAttacker(r5, pos.Them()) != NoFigure ||
			pos.GetAttacker(r6, pos.Them()) != NoFigure {
			goto EndCastleOO
		}

		*moves = append(*moves, MakeMove(Castling, r4, r6, NoPiece, ColorFigure(pos.Us(), King)))
	}
EndCastleOO:

	// Castle queen side.
	if pos.curr.CastlingAbility&ooo != 0 {
		r3 := RankFile(rank, 3)
		r2 := RankFile(rank, 2)
		r1 := RankFile(rank, 1)
		if !pos.IsEmpty(r3) || !pos.IsEmpty(r2) || !pos.IsEmpty(r1) {
			goto EndCastleOOO
		}

		r4 := RankFile(rank, 4)
		if pos.GetAttacker(r4, pos.Them()) != NoFigure ||
			pos.GetAttacker(r3, pos.Them()) != NoFigure ||
			pos.GetAttacker(r2, pos.Them()) != NoFigure {
			goto EndCastleOOO
		}

		*moves = append(*moves, MakeMove(Castling, r4, r2, NoPiece, ColorFigure(pos.Us(), King)))
	}
EndCastleOOO:
}

// GetAttacker returns the smallest figure of color them that attacks sq.
func (pos *Position) GetAttacker(sq Square, them Color) Figure {
	enemy := pos.ByColor[them]
	// Pawn
	if PawnThreats(pos, them).Has(sq) {
		return Pawn
	}
	// Knight
	if enemy&bbKnightAttack[sq]&pos.ByFigure[Knight] != 0 {
		return Knight
	}
	// Quick test of queen's attack on an empty board.
	// Exclude pawns and knights because they were already tested.
	enemy &^= pos.ByFigure[Pawn]
	enemy &^= pos.ByFigure[Knight]
	if enemy&bbSuperAttack[sq] == 0 {
		return NoFigure
	}
	// Bishop
	all := pos.ByColor[White] | pos.ByColor[Black]
	bishop := BishopMobility(sq, all)
	if enemy&pos.ByFigure[Bishop]&bishop != 0 {
		return Bishop
	}
	// Rook
	rook := RookMobility(sq, all)
	if enemy&pos.ByFigure[Rook]&rook != 0 {
		return Rook
	}
	// Queen
	if enemy&pos.ByFigure[Queen]&(bishop|rook) != 0 {
		return Queen
	}
	// King.
	if enemy&bbKingAttack[sq]&pos.ByFigure[King] != 0 {
		return King
	}
	return NoFigure
}

// GenerateMoves appends to moves all moves valid from pos.
// The generated moves are pseudo-legal, i.e. they can leave the king in check.
// kind is Quiet or Violent, or both.
func (pos *Position) GenerateMoves(kind int, moves *[]Move) {
	mask := pos.getMask(kind)
	// Order of the moves is important because the last quiet
	// moves will be reduced less.  Current order was produced
	// by testing 20 random orders and picking the best.
	pos.genKingMovesNear(mask, moves)
	pos.genPawnDoubleAdvanceMoves(kind, moves)
	pos.genRookMoves(Rook, mask, moves)
	pos.genBishopMoves(Queen, mask, moves)
	pos.genPawnAttackMoves(kind, moves)
	pos.genPawnAdvanceMoves(kind, moves)
	pos.genPawnPromotions(kind, moves)
	pos.genKnightMoves(mask, moves)
	pos.genBishopMoves(Bishop, mask, moves)
	pos.genKingCastles(kind, moves)
	pos.genRookMoves(Queen, mask, moves)
}

// GenerateFigureMoves generate moves for a given figure.
// The generated moves are pseudo-legal, i.e. they can leave the king in check.
// kind is Quiet or Violent, or both.
func (pos *Position) GenerateFigureMoves(fig Figure, kind int, moves *[]Move) {
	mask := pos.getMask(kind)
	switch fig {
	case Pawn:
		pos.genPawnAdvanceMoves(kind, moves)
		pos.genPawnAttackMoves(kind, moves)
		pos.genPawnDoubleAdvanceMoves(kind, moves)
		pos.genPawnPromotions(kind, moves)
	case Knight:
		pos.genKnightMoves(mask, moves)
	case Bishop:
		pos.genBishopMoves(Bishop, mask, moves)
	case Rook:
		pos.genRookMoves(Rook, mask, moves)
	case Queen:
		pos.genBishopMoves(Queen, mask, moves)
		pos.genRookMoves(Queen, mask, moves)
	case King:
		pos.genKingMovesNear(mask, moves)
		pos.genKingCastles(kind, moves)
	}
}

func init() {
	lostCastleRights[SquareA1] = WhiteOOO
	lostCastleRights[SquareE1] = WhiteOOO | WhiteOO
	lostCastleRights[SquareH1] = WhiteOO
	lostCastleRights[SquareA8] = BlackOOO
	lostCastleRights[SquareE8] = BlackOOO | BlackOO
	lostCastleRights[SquareH8] = BlackOO

	r := rand.New(rand.NewSource(5))
	f := func() uint64 { return uint64(r.Int63())<<32 ^ uint64(r.Int63()) }
	initZobristPiece(f)
	initZobristEnpassant(f)
	initZobristCastle(f)
	initZobristColor(f)
}

func initZobristPiece(f func() uint64) {
	for pi := PieceMinValue; pi <= PieceMaxValue; pi++ {
		for sq := SquareMinValue; sq <= SquareMaxValue; sq++ {
			zobristPiece[pi][sq] = f()
		}
	}
}

func initZobristEnpassant(f func() uint64) {
	for i := 0; i < 8; i++ {
		zobristEnpassant[SquareA3+Square(i)] = f()
		zobristEnpassant[SquareA6+Square(i)] = f()
	}
}

func initZobristCastle(f func() uint64) {
	r := [...]uint64{f(), f(), f(), f()}
	for i := CastleMinValue; i <= CastleMaxValue; i++ {
		if i&WhiteOO != 0 {
			zobristCastle[i] ^= r[0]
		}
		if i&WhiteOOO != 0 {
			zobristCastle[i] ^= r[1]
		}
		if i&BlackOO != 0 {
			zobristCastle[i] ^= r[2]
		}
		if i&BlackOOO != 0 {
			zobristCastle[i] ^= r[3]
		}
	}
}

func initZobristColor(f func() uint64) {
	zobristColor[White] = f()
}
