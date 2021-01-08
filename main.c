#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define U64 unsigned long long

#define startPosition "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 "
#define trickyPosition "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1 "
#define killerPosition "rnbqkb1r/pp1p1pPp/8/2p1pP2/1P1P4/3P3P/P1P1P3/RNBQKBNR w KQkq e6 0 1"
#define repetitions "2r3k1/R7/8/1R6/8/8/P4KPP/8 w - - 0 40 "

#define HASH_SIZE 0x400000
#define NO_HASH_ENTRY -1
#define HFLAG_EXACT 0
#define HFLAG_ALPHA 1
#define HFLAG_BETA 2

#define setBit(bitboard, square) ((bitboard) |= (1ULL << (square)))
#define getBit(bitboard, square) ((bitboard) & (1ULL << (square)))
#define popBit(bitboard, square) ((bitboard) &= ~(1ULL << (square)))

#define Move(source, target, piece, promoted, capture, doublePush, enpassant, castle) \
    (source) |          \
    (target << 6) |     \
    (piece << 12) |     \
    (promoted << 16) |  \
    (capture << 20) |   \
    (doublePush << 21) | \
    (enpassant << 22) | \
    (castle << 23)    \

#define getSource(move) (move & 0x3f)
#define getTarget(move) ((move & 0xfc0) >> 6)
#define getPiece(move) ((move & 0xf000) >> 12)
#define getPromoted(move) ((move & 0xf0000) >> 16)
#define getCapture(move) (move & 0x100000)
#define getDoublePush(move) (move & 0x200000)
#define getEnpassant(move) (move & 0x400000)
#define getCastle(move) (move & 0x800000)

enum {
    a8, b8, c8, d8, e8, f8, g8, h8,
    a7, b7, c7, d7, e7, f7, g7, h7,
    a6, b6, c6, d6, e6, f6, g6, h6,
    a5, b5, c5, d5, e5, f5, g5, h5,
    a4, b4, c4, d4, e4, f4, g4, h4,
    a3, b3, c3, d3, e3, f3, g3, h3,
    a2, b2, c2, d2, e2, f2, g2, h2,
    a1, b1, c1, d1, e1, f1, g1, h1, noSq
};

enum { white, black, both };

enum { P, N, B, R, Q, K, p, n, b, r, q, k };

enum { wk = 1, wq = 2, bk = 4, bq = 8 };

int charPieces[] = {
    ['P'] = P,
    ['N'] = N,
    ['B'] = B,
    ['R'] = R,
    ['Q'] = Q,
    ['K'] = K,
    ['p'] = p,
    ['n'] = n,
    ['b'] = b,
    ['r'] = r,
    ['q'] = q,
    ['k'] = k
};

char *unicodePieces[12] = {"♙", "♘", "♗", "♖", "♕", "♔", "♟︎", "♞", "♝", "♜", "♛", "♚"};

const int bishopRelevantBits[64] = {
	6, 5, 5, 5, 5, 5, 5, 6,
	5, 5, 5, 5, 5, 5, 5, 5,
	5, 5, 7, 7, 7, 7, 5, 5,
	5, 5, 7, 9, 9, 7, 5, 5,
	5, 5, 7, 9, 9, 7, 5, 5,
	5, 5, 7, 7, 7, 7, 5, 5,
	5, 5, 5, 5, 5, 5, 5, 5,
	6, 5, 5, 5, 5, 5, 5, 6
};

const int rookRelevantBits[64] = {
	12, 11, 11, 11, 11, 11, 11, 12,
	11, 10, 10, 10, 10, 10, 10, 11,
	11, 10, 10, 10, 10, 10, 10, 11,
	11, 10, 10, 10, 10, 10, 10, 11,
	11, 10, 10, 10, 10, 10, 10, 11,
	11, 10, 10, 10, 10, 10, 10, 11,
	11, 10, 10, 10, 10, 10, 10, 11,
	12, 11, 11, 11, 11, 11, 11, 12
};

U64 rookMagicNumbers[64] = {
	0x8a80104000800020ULL,
	0x140002000100040ULL,
	0x2801880a0017001ULL,
	0x100081001000420ULL,
	0x200020010080420ULL,
	0x3001c0002010008ULL,
	0x8480008002000100ULL,
	0x2080088004402900ULL,
	0x800098204000ULL,
	0x2024401000200040ULL,
	0x100802000801000ULL,
	0x120800800801000ULL,
	0x208808088000400ULL,
	0x2802200800400ULL,
	0x2200800100020080ULL,
	0x801000060821100ULL,
	0x80044006422000ULL,
	0x100808020004000ULL,
	0x12108a0010204200ULL,
	0x140848010000802ULL,
	0x481828014002800ULL,
	0x8094004002004100ULL,
	0x4010040010010802ULL,
	0x20008806104ULL,
	0x100400080208000ULL,
	0x2040002120081000ULL,
	0x21200680100081ULL,
	0x20100080080080ULL,
	0x2000a00200410ULL,
	0x20080800400ULL,
	0x80088400100102ULL,
	0x80004600042881ULL,
	0x4040008040800020ULL,
	0x440003000200801ULL,
	0x4200011004500ULL,
	0x188020010100100ULL,
	0x14800401802800ULL,
	0x2080040080800200ULL,
	0x124080204001001ULL,
	0x200046502000484ULL,
	0x480400080088020ULL,
	0x1000422010034000ULL,
	0x30200100110040ULL,
	0x100021010009ULL,
	0x2002080100110004ULL,
	0x202008004008002ULL,
	0x20020004010100ULL,
	0x2048440040820001ULL,
	0x101002200408200ULL,
	0x40802000401080ULL,
	0x4008142004410100ULL,
	0x2060820c0120200ULL,
	0x1001004080100ULL,
	0x20c020080040080ULL,
	0x2935610830022400ULL,
	0x44440041009200ULL,
	0x280001040802101ULL,
	0x2100190040002085ULL,
	0x80c0084100102001ULL,
	0x4024081001000421ULL,
	0x20030a0244872ULL,
	0x12001008414402ULL,
	0x2006104900a0804ULL,
	0x1004081002402ULL
};

U64 bishopMagicNumbers[64] = {
	0x40040844404084ULL,
	0x2004208a004208ULL,
	0x10190041080202ULL,
	0x108060845042010ULL,
	0x581104180800210ULL,
	0x2112080446200010ULL,
	0x1080820820060210ULL,
	0x3c0808410220200ULL,
	0x4050404440404ULL,
	0x21001420088ULL,
	0x24d0080801082102ULL,
	0x1020a0a020400ULL,
	0x40308200402ULL,
	0x4011002100800ULL,
	0x401484104104005ULL,
	0x801010402020200ULL,
	0x400210c3880100ULL,
	0x404022024108200ULL,
	0x810018200204102ULL,
	0x4002801a02003ULL,
	0x85040820080400ULL,
	0x810102c808880400ULL,
	0xe900410884800ULL,
	0x8002020480840102ULL,
	0x220200865090201ULL,
	0x2010100a02021202ULL,
	0x152048408022401ULL,
	0x20080002081110ULL,
	0x4001001021004000ULL,
	0x800040400a011002ULL,
	0xe4004081011002ULL,
	0x1c004001012080ULL,
	0x8004200962a00220ULL,
	0x8422100208500202ULL,
	0x2000402200300c08ULL,
	0x8646020080080080ULL,
	0x80020a0200100808ULL,
	0x2010004880111000ULL,
	0x623000a080011400ULL,
	0x42008c0340209202ULL,
	0x209188240001000ULL,
	0x400408a884001800ULL,
	0x110400a6080400ULL,
	0x1840060a44020800ULL,
	0x90080104000041ULL,
	0x201011000808101ULL,
	0x1a2208080504f080ULL,
	0x8012020600211212ULL,
	0x500861011240000ULL,
	0x180806108200800ULL,
	0x4000020e01040044ULL,
	0x300000261044000aULL,
	0x802241102020002ULL,
	0x20906061210001ULL,
	0x5a84841004010310ULL,
	0x4010801011c04ULL,
	0xa010109502200ULL,
	0x4a02012000ULL,
	0x500201010098b028ULL,
	0x8040002811040900ULL,
	0x28000010020204ULL,
	0x6000020202d0240ULL,
	0x8918844842082200ULL,
	0x4010011029020020ULL
};

// castling rights update constants
const int castlingRights[64] = {
     7, 15, 15, 15,  3, 15, 15, 11,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    13, 15, 15, 15, 12, 15, 15, 14
};

const U64 NOT_A_FILE = 18374403900871474942ULL;
const U64 NOT_H_FILE = 9187201950435737471ULL;
const U64 NOT_GH_FILE = 4557430888798830399ULL;
const U64 NOT_AB_FILE = 18229723555195321596ULL;

typedef struct {
	int moves[256];
	int count;
} MoveList;

typedef struct {
	U64 board[64][12];
	U64 occ[64][3];
	int ca[64];
	int ep[64];
	int side[64];
} Undo;

Undo undo[1];

U64 board[12];
U64 occ[3];

int side, castling, ep;

U64 hashKey;

U64 pieceKeys[12][64];
U64 castlingKeys[16];
U64 epKeys[64];
U64 sideKey;

U64 pawnAttacks[2][64];
U64 knightAttacks[64];
U64 kingAttacks[64];
U64 rookMasks[64];
U64 bishopMasks[64];
U64 rookAttacks[64][4096];
U64 bishopAttacks[64][512];

int ply;

long nodes;

U64 rand64()
{
	return rand() ^ ((U64)rand() << 15) ^ ((U64)rand() << 30) ^ ((U64)rand() << 45) ^ ((U64)rand() << 60);
}

void initHashKeys()
{
	for (int piece = P; piece <= k; piece++)
		for (int sq = 0; sq < 64; sq++)
			pieceKeys[piece][sq] = rand64();
			
	for (int sq = 0; sq < 64; sq++)
		epKeys[sq] = rand64();
		
	for (int i = 0; i < 16; i++)
		castlingKeys[i] = rand64();
		
	sideKey = rand64();		
}

U64 generateHashKey()
{
	U64 finalKey = 0ULL;
	
	for (int piece = P; piece <= k; piece++)
	{
		for (int sq = 0; sq < 64; sq++)
		{
			if (getBit(board[piece], sq))
			{
				finalKey ^= pieceKeys[piece][sq];
				continue;
			}
		}
	}
	
	if (ep != noSq)
		hashKey ^= epKeys[ep];
		
	hashKey ^= castlingKeys[castling];
	
	if (side == black) hashKey ^= sideKey;
	
	return finalKey;
}

void parseFen(char* fen)
{
	memset(board, 0ULL, 96);
	memset(occ, 0ULL, 24);
	
	side = white;
	castling = 0;
	ep = noSq;
	
	for (int rank = 0; rank < 8; rank++)
	{
		for (int file = 0; file < 8; file++)
		{
			int square = rank * 8 + file;
			
			if ((*fen >= 'a' && *fen <= 'z') || (*fen >= 'A' && *fen <= 'Z'))
			{
				int piece = charPieces[*fen];
				setBit(board[piece], square);
				fen++;
			}
			
			if (*fen >= '0' && *fen <= '9')
			{
				int offset = *fen - '0';
				int targetPiece = -1;
				
				for (int piece = P; piece <= k; piece++)
				{
					if (getBit(board[piece], square))
					{
						targetPiece = piece;
						break;
					}
				}
				
				if (targetPiece == -1)
					file--;
					
				file += offset;
				fen++;
			}
			
			if (*fen == '/')
				fen++;
		}
	}
	
	fen++;
	
	(*fen == 'w') ? (side = white) : (side = black);
	
	fen += 2;
	
	while (*fen != ' ')
    {
        switch (*fen)
        {
            case 'K': castling |= wk; break;
            case 'Q': castling |= wq; break;
            case 'k': castling |= bk; break;
            case 'q': castling |= bq; break;
            case '-': break;
        }
        
        fen++;
    }
    
    fen++;
    
    if (*fen != '-')
    {
        int file = fen[0] - 'a';
        int rank = 8 - (fen[1] - '0');
        
        ep = rank * 8 + file;
    }
    else
    	ep = noSq;
    	
    memset(occ, 0ULL, 24);	
    
    for (int piece = P; piece <= K; piece++)
    	occ[white] |= board[piece];
    	
    for (int piece = p; piece <= k; piece++)
    	occ[black] |= board[piece];
    	
    occ[both] = occ[white] | occ[black];
    
    hashKey = generateHashKey();
}

void printBoard()
{
	for (int sq = 0; sq < 64; sq++)
	{
		if (sq % 8 == 0)
		{
			printf("\n");
			printf("%d ", 8 - (sq / 8));
		}
		
		int isPiece = 0;
		
		for (int piece = P; piece <= k; piece++)
		{
			if (getBit(board[piece], sq))
			{
				printf("%s ", unicodePieces[piece]);
				isPiece = 1;
				break;
			}
		}
		
		if (!isPiece)
			printf(". ");
	}
	
	printf("\n  a b c d e f g h\n\n");
}

static inline int countBits(U64 bb)
{
	int count = 0;

	while (bb)
	{
		bb &= bb - 1;
		count++;
	}

	return count;
}

// gets the least significant first bit index
static inline int getLSB(U64 bb)
{
	int index = 0;

	while ((bb & 1) == 0)
	{
		index++;
		bb >>= 1;
	}

	return index;
}

U64 setOccupancy(int index, int bitsInMask, U64 attackMask)
{
	U64 occ = 0ULL;

	for (int count = 0; count < bitsInMask; count++)
	{
		int square = getLSB(attackMask);

		popBit(attackMask, square);

		if (index & (1 << count))
			occ |= (1ULL << square);
	}

	return occ;
}

U64 maskPawnAttacks(int side, int square)
{
	U64 attacks = 0ULL;
	U64 bitboard = 0ULL;
	
	setBit(bitboard, square);
	
	if (side == white)
	{
		attacks |= (bitboard >> 7) & NOT_A_FILE;
		attacks |= (bitboard >> 9) & NOT_H_FILE;
	}
	else
	{
		attacks |= (bitboard << 7) & NOT_H_FILE;
		attacks |= (bitboard << 9) & NOT_A_FILE;
	}
	
	return attacks;
}
U64 maskKnightAttacks(int sqr)
{
	U64 bitboard = 0ULL;
	U64 attacks = 0ULL;

	setBit(bitboard, sqr);

	attacks |= (bitboard >> 15) & NOT_A_FILE;
	attacks |= (bitboard >> 17) & NOT_H_FILE;
	attacks |= (bitboard >> 10) & NOT_GH_FILE;
	attacks |= (bitboard >> 6) & NOT_AB_FILE;
	attacks |= (bitboard << 15) & NOT_H_FILE;
	attacks |= (bitboard << 17) & NOT_A_FILE;
	attacks |= (bitboard << 10) & NOT_AB_FILE;
	attacks |= (bitboard << 6) & NOT_GH_FILE;

	return attacks;
}

U64 maskKingAttacks(int sqr)
{
	U64 bitboard = 0ULL;
	U64 attacks = 0ULL;

	setBit(bitboard, sqr);

	attacks |= (bitboard >> 7) & NOT_A_FILE;
	attacks |= bitboard >> 8;
	attacks |= (bitboard >> 9) & NOT_H_FILE;
	attacks |= (bitboard >> 1) & NOT_H_FILE;
	attacks |= (bitboard << 7) & NOT_H_FILE;
	attacks |= bitboard << 8;
	attacks |= (bitboard << 9) & NOT_A_FILE;
	attacks |= (bitboard << 1) & NOT_A_FILE;

	return attacks;
}

U64 maskBishopAttacks(int sqr)
{
	U64 attacks = 0ULL;

	int r, f;

	int tr = sqr / 8;
	int tf = sqr % 8;

	for (r = tr + 1, f = tf + 1; r <= 6 && f <= 6; r++, f++) attacks |= (1ULL << (r * 8 + f));
	for (r = tr - 1, f = tf + 1; r >= 1 && f <= 6; r--, f++) attacks |= (1ULL << (r * 8 + f));
	for (r = tr + 1, f = tf - 1; r <= 6 && f >= 1; r++, f--) attacks |= (1ULL << (r * 8 + f));
	for (r = tr - 1, f = tf - 1; r >= 1 && f >= 1; r--, f--) attacks |= (1ULL << (r * 8 + f));

	return attacks;
}

U64 maskRookAttacks(int sqr)
{
	U64 attacks = 0ULL;

	int r, f;

	int tr = sqr / 8;
	int tf = sqr % 8;

	for (r = tr + 1; r <= 6; r++) attacks |= (1ULL << (r * 8 + tf));
	for (r = tr - 1; r >= 1; r--) attacks |= (1ULL << (r * 8 + tf));
	for (f = tf + 1; f <= 6; f++) attacks |= (1ULL << (tr * 8 + f));
	for (f = tf - 1; f >= 1; f--) attacks |= (1ULL << (tr * 8 + f));

	return attacks;
}

U64 bishopAttacksOTF(int sqr, U64 blockers)
{
	U64 attacks = 0ULL;

	int r, f;

	int tr = sqr / 8;
	int tf = sqr % 8;

	for (r = tr + 1, f = tf + 1; r <= 7 && f <= 7; r++, f++)
	{
		attacks |= (1ULL << (r * 8 + f));
		if ((1ULL << (r * 8 + f)) & blockers) break;
	}
	for (r = tr - 1, f = tf + 1; r >= 0 && f <= 7; r--, f++)
	{
		attacks |= (1ULL << (r * 8 + f));
		if ((1ULL << (r * 8 + f)) & blockers) break;
	}
	for (r = tr + 1, f = tf - 1; r <= 7 && f >= 0; r++, f--)
	{
		attacks |= (1ULL << (r * 8 + f));
		if ((1ULL << (r * 8 + f)) & blockers) break;
	}
	for (r = tr - 1, f = tf - 1; r >= 0 && f >= 0; r--, f--)
	{
		attacks |= (1ULL << (r * 8 + f));
		if ((1ULL << (r * 8 + f)) & blockers) break;
	}

	return attacks;
}

U64 rookAttacksOTF(int square, U64 blockers)
{
	U64 attacks = 0ULL;

	int r, f;

	int tr = square / 8;
	int tf = square % 8;

	for (r = tr + 1; r <= 7; r++)
	{
		attacks |= (1ULL << (r * 8 + tf));
		if ((1ULL << (r * 8 + tf)) & blockers) break;
	}

	for (r = tr - 1; r >= 0; r--)
	{
		attacks |= (1ULL << (r * 8 + tf));
		if ((1ULL << (r * 8 + tf)) & blockers) break;
	}

	for (f = tf + 1; f <= 7; f++)
	{
		attacks |= (1ULL << (tr * 8 + f));
		if ((1ULL << (tr * 8 + f)) & blockers) break;
	}

	for (f = tf - 1; f >= 0; f--)
	{
		attacks |= (1ULL << (tr * 8 + f));
		if ((1ULL << (tr * 8 + f)) & blockers) break;
	}

	return attacks;
}

static inline U64 getRookAttacks(int sqr, U64 occ)
{
	occ &= rookMasks[sqr];
	occ *= rookMagicNumbers[sqr];
	occ >>= 64 - rookRelevantBits[sqr];

	return rookAttacks[sqr][occ];
}

static inline U64 getBishopAttacks(int sqr, U64 occ)
{
	occ &= bishopMasks[sqr];
	occ *= bishopMagicNumbers[sqr];
	occ >>= 64 - bishopRelevantBits[sqr];

	return bishopAttacks[sqr][occ];
}

static inline U64 getQueenAttacks(int sqr, U64 occ) 
{ 
	return getBishopAttacks(sqr, occ) | getRookAttacks(sqr, occ); 
}

void initAttackMasks()
{
	for (int sq = 0; sq < 64; sq++)
	{
		pawnAttacks[white][sq] = maskPawnAttacks(white, sq);
		pawnAttacks[black][sq] = maskPawnAttacks(black, sq);
		knightAttacks[sq] = maskKnightAttacks(sq);
		kingAttacks[sq] = maskKingAttacks(sq);
		bishopMasks[sq] = maskBishopAttacks(sq);
		rookMasks[sq] = maskRookAttacks(sq);
	}
}

void initSliderAttacks(int isBishop)
{
	for (int sqr = 0; sqr < 64; sqr++)
	{
		U64 attackMask = isBishop ? bishopMasks[sqr] : rookMasks[sqr];

		int relevantBitsCount = countBits(attackMask);

		int occupancyIndices = (1 << relevantBitsCount);

		for (int index = 0; index < occupancyIndices; index++)
		{
			if (isBishop)
			{
				U64 occ = setOccupancy(index, relevantBitsCount, attackMask);
				int magic_index = (occ * bishopMagicNumbers[sqr]) >> (64 - bishopRelevantBits[sqr]);
				bishopAttacks[sqr][magic_index] = bishopAttacksOTF(sqr, occ);
			}
			else
			{
				U64 occ = setOccupancy(index, relevantBitsCount, attackMask);
				int magic_index = (occ * rookMagicNumbers[sqr]) >> (64 - rookRelevantBits[sqr]);
				rookAttacks[sqr][magic_index] = rookAttacksOTF(sqr, occ);
			}
		}
	}
}

static inline int isSqAttacked(int square, int side)
{
	if (side == white && (pawnAttacks[black][square] & board[P])) return 1;
	if (side == black && (pawnAttacks[white][square] & board[p])) return 1;
	if (knightAttacks[square] & (side == white ? board[N] : board[n])) return 1;
	if (kingAttacks[square] & (side == white ? board[K] : board[k])) return 1;
	if (getBishopAttacks(square, occ[both]) & (side == white ? board[B] : board[b])) return 1;
	if (getRookAttacks(square, occ[both]) & (side == white ? board[R] : board[r])) return 1;
	if (getQueenAttacks(square, occ[both]) & ((side == white ? board[Q] : board[q]))) return 1;

	return 0;
}

static inline void copyBoard()
{
	undo->ca[ply] = castling, undo->ep[ply] = ep, undo->side[ply] = side;
	memcpy(undo->board[ply], board, 96);
	memcpy(undo->occ[ply], occ, 24);
}

static inline void takeBack()
{
	memcpy(board, undo->board[ply], 96);
	memcpy(occ, undo-occ[ply], 24);
	castling = undo->ca[ply], ep = undo->ep[ply], side = undo->side[ply];
}

static inline void addMove(MoveList* moves, int move)
{
	moves->moves[moves->count++] = move;
}

static inline void generateMoves(MoveList* moves)
{
	int fromSquare, toSquare;

	U64 bitboard, attacks;

	moves->count = 0;

	for (int piece = P; piece <= k; piece++)
	{
		bitboard = board[piece];

		if (side == white)
		{
			if (piece == P)
			{
				while (bitboard)
				{
					fromSquare = getLSB(bitboard);
					toSquare = fromSquare - 8;

					// generate quiet pawn moves
					if (!getBit(occ[both], toSquare))
					{
						// promotion
						if (fromSquare >= a7 && fromSquare <= h7)
						{
							addMove(moves, Move(fromSquare, toSquare, piece, Q, 0, 0, 0, 0));
							addMove(moves, Move(fromSquare, toSquare, piece, R, 0, 0, 0, 0));
							addMove(moves, Move(fromSquare, toSquare, piece, B, 0, 0, 0, 0));
							addMove(moves, Move(fromSquare, toSquare, piece, N, 0, 0, 0, 0));
						}
						else
						{
							// single pawn push
							addMove(moves, Move(fromSquare, toSquare, piece, 0, 0, 0, 0, 0));

							// double pawn push
							if ((fromSquare >= a2 && fromSquare <= h2) && !getBit(occ[both], toSquare - 8))
								addMove(moves, Move(fromSquare, toSquare - 8, piece, 0, 0, 1, 0, 0));
						}
					}

					attacks = pawnAttacks[side][fromSquare] & occ[black];

					// generate pawn captures
					while (attacks)
					{
						toSquare = getLSB(attacks);

						if (fromSquare >= a7 && fromSquare <= h7)
						{
							addMove(moves, Move(fromSquare, toSquare, piece, Q, 1, 0, 0, 0));
							addMove(moves, Move(fromSquare, toSquare, piece, R, 1, 0, 0, 0));
							addMove(moves, Move(fromSquare, toSquare, piece, B, 1, 0, 0, 0));
							addMove(moves, Move(fromSquare, toSquare, piece, N, 1, 0, 0, 0));
						}
						else
							addMove(moves, Move(fromSquare, toSquare, piece, 0, 1, 0, 0, 0));

						popBit(attacks, toSquare);
					}

					// generate enpassant captures
					if (ep != noSq)
					{
						U64 enpassant_attacks = pawnAttacks[side][fromSquare] & (1ULL << ep);

						if (enpassant_attacks)
						{
							int target_enpassant = getLSB(enpassant_attacks);
							addMove(moves, Move(fromSquare, target_enpassant, piece, 0, 1, 0, 1, 0));
						}
					}

					popBit(bitboard, fromSquare);
				}
			}
			else if (piece == K)
			{
				if (castling & wk)
				{
					if (!getBit(occ[both], f1) && !getBit(occ[both], g1))
					{
						if (!isSqAttacked(e1, black) && !isSqAttacked(f1, black))
							addMove(moves, Move(e1, g1, piece, 0, 0, 0, 0, 1));
					}
				}
				else if (castling & wq)
				{
					if (!getBit(occ[both], d1) && !getBit(occ[both], c1) && !getBit(occ[both], b1))
					{
						if (!isSqAttacked(e1, black) && !isSqAttacked(d1, black))
							addMove(moves, Move(e1, c1, piece, 0, 0, 0, 0, 1));
					}
				}
			}
		}
		else
		{
			if (piece == p)
			{
				while (bitboard)
				{
					fromSquare = getLSB(bitboard);
					toSquare = fromSquare + 8;


					// generate quiet pawn moves
					if (!getBit(occ[both], toSquare))
					{
						if (fromSquare >= a2 && fromSquare <= h2)
						{
							addMove(moves, Move(fromSquare, toSquare, piece, q, 0, 0, 0, 0));
							addMove(moves, Move(fromSquare, toSquare, piece, r, 0, 0, 0, 0));
							addMove(moves, Move(fromSquare, toSquare, piece, b, 0, 0, 0, 0));
							addMove(moves, Move(fromSquare, toSquare, piece, n, 0, 0, 0, 0));
						}
						else
						{
							// generate single pawn pushes
							addMove(moves, Move(fromSquare, toSquare, piece, 0, 0, 0, 0, 0));

							// generate double pawn pushes
							if ((fromSquare >= a7 && fromSquare <= h7) && !getBit(occ[both], toSquare + 8))
								addMove(moves, Move(fromSquare, toSquare + 8, piece, 0, 0, 1, 0, 0));
						}
					}

					attacks = pawnAttacks[side][fromSquare] & occ[white];

					// generate pawn captures
					while (attacks)
					{
						toSquare = getLSB(attacks);

						if (fromSquare >= a2 && fromSquare <= h2)
						{
							addMove(moves, Move(fromSquare, toSquare, piece, q, 1, 0, 0, 0));
							addMove(moves, Move(fromSquare, toSquare, piece, r, 1, 0, 0, 0));
							addMove(moves, Move(fromSquare, toSquare, piece, b, 1, 0, 0, 0));
							addMove(moves, Move(fromSquare, toSquare, piece, n, 1, 0, 0, 0));
						}
						else
							addMove(moves, Move(fromSquare, toSquare, piece, 0, 1, 0, 0, 0));

						popBit(attacks, toSquare);
					}

					// generate enpassant captures
					if (ep != noSq)
					{
						U64 enpassant_attacks = pawnAttacks[side][fromSquare] & (1ULL << ep);

						if (enpassant_attacks)
						{
							int target_enpassant = getLSB(enpassant_attacks);
							addMove(moves, Move(fromSquare, target_enpassant, piece, 0, 1, 0, 1, 0));
						}
					}

					popBit(bitboard, fromSquare);
				}
			}
			else if (piece == k)
			{
				if (castling & bk)
				{
					if (!getBit(occ[both], f8) && !getBit(occ[both], g8))
						if (!isSqAttacked(e8, white) && !isSqAttacked(f8, white))
							addMove(moves, Move(e8, g8, piece, 0, 0, 0, 0, 1));
				}
				else if (castling & bq)
				{
					if (!getBit(occ[both], d8) && !getBit(occ[both], c8) && !getBit(occ[both], b8))
						if (!isSqAttacked(e8, white) && !isSqAttacked(d8, white))
							addMove(moves, Move(e8, c8, piece, 0, 0, 0, 0, 1));
				}
			}
		}

		// generate knight moves
		if ((side == white) ? piece == N : piece == n)
		{
			while (bitboard)
			{
				fromSquare = getLSB(bitboard);

				attacks = knightAttacks[fromSquare] & ((side == white) ? ~occ[white] : ~occ[black]);

				while (attacks)
				{
					toSquare = getLSB(attacks);

					// quiet moves
					if (!getBit(((side == white) ? occ[black] : occ[white]), toSquare))
						addMove(moves, Move(fromSquare, toSquare, piece, 0, 0, 0, 0, 0));
					// capture moves
					else
						addMove(moves, Move(fromSquare, toSquare, piece, 0, 1, 0, 0, 0));

					popBit(attacks, toSquare);
				}

				popBit(bitboard, fromSquare);
			}
		}

		// generate bishop moves
		if ((side == white) ? piece == B : piece == b)
		{
			while (bitboard)
			{
				fromSquare = getLSB(bitboard);

				attacks = getBishopAttacks(fromSquare, occ[both]) & ((side == white) ? ~occ[white] : ~occ[black]);

				while (attacks)
				{
					toSquare = getLSB(attacks);

					// quiet moves
					if (!getBit(((side == white) ? occ[black] : occ[white]), toSquare))
						addMove(moves, Move(fromSquare, toSquare, piece, 0, 0, 0, 0, 0));
					// capture moves
					else
						addMove(moves, Move(fromSquare, toSquare, piece, 0, 1, 0, 0, 0));

					popBit(attacks, toSquare);
				}

				popBit(bitboard, fromSquare);
			}
		}

		// generate rook moves
		if ((side == white) ? piece == R : piece == r)
		{
			while (bitboard)
			{
				fromSquare = getLSB(bitboard);

				attacks = getRookAttacks(fromSquare, occ[both]) & ((side == white) ? ~occ[white] : ~occ[black]);

				while (attacks)
				{
					toSquare = getLSB(attacks);

					// quiet moves
					if (!getBit(((side == white) ? occ[black] : occ[white]), toSquare))
						addMove(moves, Move(fromSquare, toSquare, piece, 0, 0, 0, 0, 0));
					// capture moves
					else
						addMove(moves, Move(fromSquare, toSquare, piece, 0, 1, 0, 0, 0));

					popBit(attacks, toSquare);
				}

				popBit(bitboard, fromSquare);
			}
		}

		// generate queen moves
		if ((side == white) ? piece == Q : piece == q)
		{
			while (bitboard)
			{
				fromSquare = getLSB(bitboard);

				attacks = getQueenAttacks(fromSquare, occ[both]) & ((side == white) ? ~occ[white] : ~occ[black]);

				while (attacks)
				{
					toSquare = getLSB(attacks);

					// quiet moves
					if (!getBit(((side == white) ? occ[black] : occ[white]), toSquare))
						addMove(moves, Move(fromSquare, toSquare, piece, 0, 0, 0, 0, 0));
					// capture moves
					else
						addMove(moves, Move(fromSquare, toSquare, piece, 0, 1, 0, 0, 0));

					popBit(attacks, toSquare);
				}

				popBit(bitboard, fromSquare);
			}
		}

		// generate king moves
		if ((side == white) ? piece == K : piece == k)
		{
			while (bitboard)
			{
				fromSquare = getLSB(bitboard);

				attacks = kingAttacks[fromSquare] & ((side == white) ? ~occ[white] : ~occ[black]);

				while (attacks)
				{
					toSquare = getLSB(attacks);

					// quiet moves
					if (!getBit(((side == white) ? occ[black] : occ[white]), toSquare))
						addMove(moves, Move(fromSquare, toSquare, piece, 0, 0, 0, 0, 0));
					// capture moves
					else
						addMove(moves, Move(fromSquare, toSquare, piece, 0, 1, 0, 0, 0));

					popBit(attacks, toSquare);
				}

				popBit(bitboard, fromSquare);
			}
		}
	}
}

static inline int makeMove(int move, int onlyCaptures)
{
	if (!onlyCaptures)
	{
		int fromSquare = getSource(move);
		int toSquare = getTarget(move);
		int piece = getPiece(move);
		int promotedPiece = getPromoted(move);
		int capture = getCapture(move);
		int doublePush = getDoublePush(move);
		int enpass = getEnpassant(move);
		int castle = getCastle(move);

		copyBoard();

		popBit(board[piece], fromSquare);
		setBit(board[piece], toSquare);
		
		hashKey ^= pieceKeys[piece][fromSquare];
		hashKey ^= pieceKeys[piece][toSquare];

		if (capture)
		{
			int startPiece, endPiece;

			if (side == white)
			{
				startPiece = p;
				endPiece = k;
			}
			else
			{
				startPiece = P;
				endPiece = K;
			}
				
			for (int bbPiece = startPiece; bbPiece <= endPiece; bbPiece++)
			{
				if (getBit(board[bbPiece], toSquare))
				{
					popBit(board[bbPiece], toSquare);
					hashKey ^= pieceKeys[bbPiece][toSquare];
					break;
				}
			}
		}

		if (promotedPiece)
		{
			(side == white) ? popBit(board[P], toSquare) : popBit(board[p], toSquare);
			(side == white) ? (hashKey ^= pieceKeys[P][toSquare]) : (hashKey ^= pieceKeys[p][toSquare]);
			setBit(board[promotedPiece], toSquare);
			hashKey ^= pieceKeys[promotedPiece][toSquare];
		}

		if (enpass)
		{
			side == white ? popBit(board[p], toSquare + 8) : popBit(board[P], toSquare - 8);
			side == white ? (hashKey ^= pieceKeys[p][toSquare + 8]) : (hashKey ^= pieceKeys[P][toSquare - 8]);
		}
		
		if (ep != noSq)
			hashKey ^= epKeys[ep];

		ep = noSq;

		if (doublePush)
		{
			if (side == white)
			{
				ep = toSquare + 8;
				hashKey ^= epKeys[toSquare + 8];
			}
			else
			{
				ep = toSquare - 8;
				hashKey ^= epKeys[toSquare - 8];
			}
		}

		if (castle)
		{
		    switch (toSquare)
		    {
		        // white castles king side
		        case g1:
		            // move H rook
		            popBit(board[R], h1);
		            setBit(board[R], f1);
		            hashKey ^= pieceKeys[R][h1];
		            hashKey ^= pieceKeys[R][f1];
		            break;
		            
		        // white castles queen side
		        case c1:
		            // move A rook
		            popBit(board[R], a1);
		            setBit(board[R], d1);
		            hashKey ^= pieceKeys[R][a1];
		            hashKey ^= pieceKeys[R][d1];
		            break;
		            
		        // black castles king side
		        case g8:
		            // move H rook
		            popBit(board[r], h8);
		            setBit(board[r], f8);
		           	hashKey ^= pieceKeys[r][h8];
		            hashKey ^= pieceKeys[r][f8];
		            break;
		            
		        // black castles queen side
		        case c8:
		            // move A rook
		            popBit(board[r], a8);
		            setBit(board[r], d8);
		            hashKey ^= pieceKeys[r][a8];
		            hashKey ^= pieceKeys[r][d8];
		            break;
		    }
		}

		//hashKey ^= castlingKeys[castling];

		castling &= castlingRights[fromSquare];
		castling &= castlingRights[toSquare];
		
		hashKey ^= castlingKeys[castling];

		memset(occ, 0, 24);

		for (int bb_piece = P; bb_piece <= K; bb_piece++)
		    occ[white] |= board[bb_piece];

		for (int bb_piece = p; bb_piece <= k; bb_piece++)
		    occ[black] |= board[bb_piece];

		occ[both] = occ[white] | occ[black];

		side ^= 1;
		hashKey ^= sideKey;

		if (isSqAttacked(side == white ? getLSB(board[k]) : getLSB(board[K]), side))
		{
			takeBack();
			return 0;
		}
		else
			return 1;
	}
	else
	{
		if (getCapture(move))
		{
			makeMove(move, 0);
			return 1;
		}
		else
			return 0;
	}
}

static inline void perft(int depth)
{
	if (depth == 0)
	{
		nodes++;
		return;
	}
	
	MoveList moves[1];
	generateMoves(moves);
	
	for (int i = 0; i < moves->count; i++)
	{
		copyBoard();
		
		if (!makeMove(moves->moves[i], 0))
			continue;
			
		ply++;
		perft(depth - 1);
		ply--;
		
		takeBack();
	}
}

void initAll()
{
	initHashKeys();
	initAttackMasks();
	initSliderAttacks(1);
	initSliderAttacks(0);
}

int main()
{
	initAll();

	parseFen(startPosition);
	printBoard();
	
	perft(1);
	printf("Nodes: %ld\n", nodes);

	return 0;
}
