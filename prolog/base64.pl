% see https://en.wikipedia.org/wiki/Base64 for a description of how base64 works
:- module(pbase64, [
    codes_base64/2,
    base64_codes/2
]).
	
codes_base64([],[]).
codes_base64([C1],[O1,O2,'=','=']) :-
	code_byte( C1, byte(B0, B1, B2, B3, B4, B5, B6, B7) ),
	code_b64( [B0, B1, B2, B3, B4, B5 ], O1 ),
	code_b64( [B6, B7, 0,  0,  0,  0  ], O2 ).
	
codes_base64([C1,C2],[O1,O2,O3,'=']) :-
    code_byte( C1, byte(B0, B1, B2, B3, B4, B5, B6, B7) ),
	code_byte( C2, byte(B8, B9, B10,B11,B12,B13,B14,B15) ),

	code_b64( [B0, B1, B2, B3, B4, B5 ], O1 ),
	code_b64( [B6, B7, B8, B9, B10,B11], O2 ),
	code_b64( [B12,B13,B14,B15,0,  0  ], O3 ).

codes_base64([C1,C2,C3|Ct],[O1,O2,O3,O4|Ot]) :-
	code_byte( C1, byte(B0, B1, B2, B3, B4, B5, B6, B7) ),
	code_byte( C2, byte(B8, B9, B10,B11,B12,B13,B14,B15) ),
	code_byte( C3, byte(B16,B17,B18,B19,B20,B21,B22,B23) ),

	code_b64( [B0, B1, B2, B3, B4, B5 ], O1 ),	
	code_b64( [B6, B7, B8, B9, B10,B11], O2 ),
	code_b64( [B12,B13,B14,B15,B16,B17], O3 ),	
	code_b64( [B18,B19,B20,B21,B22,B23], O4 ),
	
	codes_base64(Ct,Ot).

base64_codes([],[]).
base64_codes([C1],[O1,O2,'=','=']) :-
	code_b64( [B0, B1, B2, B3, B4, B5 ], O1 ),
	code_b64( [B6, B7, 0,  0,  0,  0  ], O2 ),
	code_byte( C1, byte(B0, B1, B2, B3, B4, B5, B6, B7) ).

base64_codes([C1,C2],[O1,O2,O3,'=']) :-
	code_b64( [B0, B1, B2, B3, B4, B5 ], O1 ),
	code_b64( [B6, B7, B8, B9, B10,B11], O2 ),
	code_b64( [B12,B13,B14,B15,0,  0  ], O3 ),

    code_byte( C1, byte(B0, B1, B2, B3, B4, B5, B6, B7) ),
	code_byte( C2, byte(B8, B9, B10,B11,B12,B13,B14,B15) ).

base64_codes([C1,C2,C3|Ct],[O1,O2,O3,O4|Ot]) :-
	code_b64( [B0, B1, B2, B3, B4, B5 ], O1 ),	
	code_b64( [B6, B7, B8, B9, B10,B11], O2 ),
	code_b64( [B12,B13,B14,B15,B16,B17], O3 ),	
	code_b64( [B18,B19,B20,B21,B22,B23], O4 ),
	
	code_byte( C1, byte(B0, B1, B2, B3, B4, B5, B6, B7) ),
	code_byte( C2, byte(B8, B9, B10,B11,B12,B13,B14,B15) ),
	code_byte( C3, byte(B16,B17,B18,B19,B20,B21,B22,B23) ),

	base64_codes(Ct,Ot).


code_byte( 0,   byte(0,0,0,0,0,0,0,0) ).
code_byte( 1,   byte(0,0,0,0,0,0,0,1) ).
code_byte( 2,   byte(0,0,0,0,0,0,1,0) ).
code_byte( 3,   byte(0,0,0,0,0,0,1,1) ).
code_byte( 4,   byte(0,0,0,0,0,1,0,0) ).
code_byte( 5,   byte(0,0,0,0,0,1,0,1) ).
code_byte( 6,   byte(0,0,0,0,0,1,1,0) ).
code_byte( 7,   byte(0,0,0,0,0,1,1,1) ).
code_byte( 8,   byte(0,0,0,0,1,0,0,0) ).
code_byte( 9,   byte(0,0,0,0,1,0,0,1) ).
code_byte( 10,  byte(0,0,0,0,1,0,1,0) ).
code_byte( 11,  byte(0,0,0,0,1,0,1,1) ).
code_byte( 12,  byte(0,0,0,0,1,1,0,0) ).
code_byte( 13,  byte(0,0,0,0,1,1,0,1) ).
code_byte( 14,  byte(0,0,0,0,1,1,1,0) ).
code_byte( 15,  byte(0,0,0,0,1,1,1,1) ).
code_byte( 16,  byte(0,0,0,1,0,0,0,0) ).
code_byte( 17,  byte(0,0,0,1,0,0,0,1) ).
code_byte( 18,  byte(0,0,0,1,0,0,1,0) ).
code_byte( 19,  byte(0,0,0,1,0,0,1,1) ).
code_byte( 20,  byte(0,0,0,1,0,1,0,0) ).
code_byte( 21,  byte(0,0,0,1,0,1,0,1) ).
code_byte( 22,  byte(0,0,0,1,0,1,1,0) ).
code_byte( 23,  byte(0,0,0,1,0,1,1,1) ).
code_byte( 24,  byte(0,0,0,1,1,0,0,0) ).
code_byte( 25,  byte(0,0,0,1,1,0,0,1) ).
code_byte( 26,  byte(0,0,0,1,1,0,1,0) ).
code_byte( 27,  byte(0,0,0,1,1,0,1,1) ).
code_byte( 28,  byte(0,0,0,1,1,1,0,0) ).
code_byte( 29,  byte(0,0,0,1,1,1,0,1) ).
code_byte( 30,  byte(0,0,0,1,1,1,1,0) ).
code_byte( 31,  byte(0,0,0,1,1,1,1,1) ).
code_byte( 32,  byte(0,0,1,0,0,0,0,0) ).
code_byte( 33,  byte(0,0,1,0,0,0,0,1) ).
code_byte( 34,  byte(0,0,1,0,0,0,1,0) ).
code_byte( 35,  byte(0,0,1,0,0,0,1,1) ).
code_byte( 36,  byte(0,0,1,0,0,1,0,0) ).
code_byte( 37,  byte(0,0,1,0,0,1,0,1) ).
code_byte( 38,  byte(0,0,1,0,0,1,1,0) ).
code_byte( 39,  byte(0,0,1,0,0,1,1,1) ).
code_byte( 40,  byte(0,0,1,0,1,0,0,0) ).
code_byte( 41,  byte(0,0,1,0,1,0,0,1) ).
code_byte( 42,  byte(0,0,1,0,1,0,1,0) ).
code_byte( 43,  byte(0,0,1,0,1,0,1,1) ).
code_byte( 44,  byte(0,0,1,0,1,1,0,0) ).
code_byte( 45,  byte(0,0,1,0,1,1,0,1) ).
code_byte( 46,  byte(0,0,1,0,1,1,1,0) ).
code_byte( 47,  byte(0,0,1,0,1,1,1,1) ).
code_byte( 48,  byte(0,0,1,1,0,0,0,0) ).
code_byte( 49,  byte(0,0,1,1,0,0,0,1) ).
code_byte( 50,  byte(0,0,1,1,0,0,1,0) ).
code_byte( 51,  byte(0,0,1,1,0,0,1,1) ).
code_byte( 52,  byte(0,0,1,1,0,1,0,0) ).
code_byte( 53,  byte(0,0,1,1,0,1,0,1) ).
code_byte( 54,  byte(0,0,1,1,0,1,1,0) ).
code_byte( 55,  byte(0,0,1,1,0,1,1,1) ).
code_byte( 56,  byte(0,0,1,1,1,0,0,0) ).
code_byte( 57,  byte(0,0,1,1,1,0,0,1) ).
code_byte( 58,  byte(0,0,1,1,1,0,1,0) ).
code_byte( 59,  byte(0,0,1,1,1,0,1,1) ).
code_byte( 60,  byte(0,0,1,1,1,1,0,0) ).
code_byte( 61,  byte(0,0,1,1,1,1,0,1) ).
code_byte( 62,  byte(0,0,1,1,1,1,1,0) ).
code_byte( 63,  byte(0,0,1,1,1,1,1,1) ).
code_byte( 64,  byte(0,1,0,0,0,0,0,0) ).
code_byte( 65,  byte(0,1,0,0,0,0,0,1) ).
code_byte( 66,  byte(0,1,0,0,0,0,1,0) ).
code_byte( 67,  byte(0,1,0,0,0,0,1,1) ).
code_byte( 68,  byte(0,1,0,0,0,1,0,0) ).
code_byte( 69,  byte(0,1,0,0,0,1,0,1) ).
code_byte( 70,  byte(0,1,0,0,0,1,1,0) ).
code_byte( 71,  byte(0,1,0,0,0,1,1,1) ).
code_byte( 72,  byte(0,1,0,0,1,0,0,0) ).
code_byte( 73,  byte(0,1,0,0,1,0,0,1) ).
code_byte( 74,  byte(0,1,0,0,1,0,1,0) ).
code_byte( 75,  byte(0,1,0,0,1,0,1,1) ).
code_byte( 76,  byte(0,1,0,0,1,1,0,0) ).
code_byte( 77,  byte(0,1,0,0,1,1,0,1) ).
code_byte( 78,  byte(0,1,0,0,1,1,1,0) ).
code_byte( 79,  byte(0,1,0,0,1,1,1,1) ).
code_byte( 80,  byte(0,1,0,1,0,0,0,0) ).
code_byte( 81,  byte(0,1,0,1,0,0,0,1) ).
code_byte( 82,  byte(0,1,0,1,0,0,1,0) ).
code_byte( 83,  byte(0,1,0,1,0,0,1,1) ).
code_byte( 84,  byte(0,1,0,1,0,1,0,0) ).
code_byte( 85,  byte(0,1,0,1,0,1,0,1) ).
code_byte( 86,  byte(0,1,0,1,0,1,1,0) ).
code_byte( 87,  byte(0,1,0,1,0,1,1,1) ).
code_byte( 88,  byte(0,1,0,1,1,0,0,0) ).
code_byte( 89,  byte(0,1,0,1,1,0,0,1) ).
code_byte( 90,  byte(0,1,0,1,1,0,1,0) ).
code_byte( 91,  byte(0,1,0,1,1,0,1,1) ).
code_byte( 92,  byte(0,1,0,1,1,1,0,0) ).
code_byte( 93,  byte(0,1,0,1,1,1,0,1) ).
code_byte( 94,  byte(0,1,0,1,1,1,1,0) ).
code_byte( 95,  byte(0,1,0,1,1,1,1,1) ).
code_byte( 96,  byte(0,1,1,0,0,0,0,0) ).
code_byte( 97,  byte(0,1,1,0,0,0,0,1) ).
code_byte( 98,  byte(0,1,1,0,0,0,1,0) ).
code_byte( 99,  byte(0,1,1,0,0,0,1,1) ).
code_byte( 100, byte(0,1,1,0,0,1,0,0) ).
code_byte( 101, byte(0,1,1,0,0,1,0,1) ).
code_byte( 102, byte(0,1,1,0,0,1,1,0) ).
code_byte( 103, byte(0,1,1,0,0,1,1,1) ).
code_byte( 104, byte(0,1,1,0,1,0,0,0) ).
code_byte( 105, byte(0,1,1,0,1,0,0,1) ).
code_byte( 106, byte(0,1,1,0,1,0,1,0) ).
code_byte( 107, byte(0,1,1,0,1,0,1,1) ).
code_byte( 108, byte(0,1,1,0,1,1,0,0) ).
code_byte( 109, byte(0,1,1,0,1,1,0,1) ).
code_byte( 110, byte(0,1,1,0,1,1,1,0) ).
code_byte( 111, byte(0,1,1,0,1,1,1,1) ).
code_byte( 112, byte(0,1,1,1,0,0,0,0) ).
code_byte( 113, byte(0,1,1,1,0,0,0,1) ).
code_byte( 114, byte(0,1,1,1,0,0,1,0) ).
code_byte( 115, byte(0,1,1,1,0,0,1,1) ).
code_byte( 116, byte(0,1,1,1,0,1,0,0) ).
code_byte( 117, byte(0,1,1,1,0,1,0,1) ).
code_byte( 118, byte(0,1,1,1,0,1,1,0) ).
code_byte( 119, byte(0,1,1,1,0,1,1,1) ).
code_byte( 120, byte(0,1,1,1,1,0,0,0) ).
code_byte( 121, byte(0,1,1,1,1,0,0,1) ).
code_byte( 122, byte(0,1,1,1,1,0,1,0) ).
code_byte( 123, byte(0,1,1,1,1,0,1,1) ).
code_byte( 124, byte(0,1,1,1,1,1,0,0) ).
code_byte( 125, byte(0,1,1,1,1,1,0,1) ).
code_byte( 126, byte(0,1,1,1,1,1,1,0) ).
code_byte( 127, byte(0,1,1,1,1,1,1,1) ).
code_byte( 128, byte(1,0,0,0,0,0,0,0) ).
code_byte( 129, byte(1,0,0,0,0,0,0,1) ).
code_byte( 130, byte(1,0,0,0,0,0,1,0) ).
code_byte( 131, byte(1,0,0,0,0,0,1,1) ).
code_byte( 132, byte(1,0,0,0,0,1,0,0) ).
code_byte( 133, byte(1,0,0,0,0,1,0,1) ).
code_byte( 134, byte(1,0,0,0,0,1,1,0) ).
code_byte( 135, byte(1,0,0,0,0,1,1,1) ).
code_byte( 136, byte(1,0,0,0,1,0,0,0) ).
code_byte( 137, byte(1,0,0,0,1,0,0,1) ).
code_byte( 138, byte(1,0,0,0,1,0,1,0) ).
code_byte( 139, byte(1,0,0,0,1,0,1,1) ).
code_byte( 140, byte(1,0,0,0,1,1,0,0) ).
code_byte( 141, byte(1,0,0,0,1,1,0,1) ).
code_byte( 142, byte(1,0,0,0,1,1,1,0) ).
code_byte( 143, byte(1,0,0,0,1,1,1,1) ).
code_byte( 144, byte(1,0,0,1,0,0,0,0) ).
code_byte( 145, byte(1,0,0,1,0,0,0,1) ).
code_byte( 146, byte(1,0,0,1,0,0,1,0) ).
code_byte( 147, byte(1,0,0,1,0,0,1,1) ).
code_byte( 148, byte(1,0,0,1,0,1,0,0) ).
code_byte( 149, byte(1,0,0,1,0,1,0,1) ).
code_byte( 150, byte(1,0,0,1,0,1,1,0) ).
code_byte( 151, byte(1,0,0,1,0,1,1,1) ).
code_byte( 152, byte(1,0,0,1,1,0,0,0) ).
code_byte( 153, byte(1,0,0,1,1,0,0,1) ).
code_byte( 154, byte(1,0,0,1,1,0,1,0) ).
code_byte( 155, byte(1,0,0,1,1,0,1,1) ).
code_byte( 156, byte(1,0,0,1,1,1,0,0) ).
code_byte( 157, byte(1,0,0,1,1,1,0,1) ).
code_byte( 158, byte(1,0,0,1,1,1,1,0) ).
code_byte( 159, byte(1,0,0,1,1,1,1,1) ).
code_byte( 160, byte(1,0,1,0,0,0,0,0) ).
code_byte( 161, byte(1,0,1,0,0,0,0,1) ).
code_byte( 162, byte(1,0,1,0,0,0,1,0) ).
code_byte( 163, byte(1,0,1,0,0,0,1,1) ).
code_byte( 164, byte(1,0,1,0,0,1,0,0) ).
code_byte( 165, byte(1,0,1,0,0,1,0,1) ).
code_byte( 166, byte(1,0,1,0,0,1,1,0) ).
code_byte( 167, byte(1,0,1,0,0,1,1,1) ).
code_byte( 168, byte(1,0,1,0,1,0,0,0) ).
code_byte( 169, byte(1,0,1,0,1,0,0,1) ).
code_byte( 170, byte(1,0,1,0,1,0,1,0) ).
code_byte( 171, byte(1,0,1,0,1,0,1,1) ).
code_byte( 172, byte(1,0,1,0,1,1,0,0) ).
code_byte( 173, byte(1,0,1,0,1,1,0,1) ).
code_byte( 174, byte(1,0,1,0,1,1,1,0) ).
code_byte( 175, byte(1,0,1,0,1,1,1,1) ).
code_byte( 176, byte(1,0,1,1,0,0,0,0) ).
code_byte( 177, byte(1,0,1,1,0,0,0,1) ).
code_byte( 178, byte(1,0,1,1,0,0,1,0) ).
code_byte( 179, byte(1,0,1,1,0,0,1,1) ).
code_byte( 180, byte(1,0,1,1,0,1,0,0) ).
code_byte( 181, byte(1,0,1,1,0,1,0,1) ).
code_byte( 182, byte(1,0,1,1,0,1,1,0) ).
code_byte( 183, byte(1,0,1,1,0,1,1,1) ).
code_byte( 184, byte(1,0,1,1,1,0,0,0) ).
code_byte( 185, byte(1,0,1,1,1,0,0,1) ).
code_byte( 186, byte(1,0,1,1,1,0,1,0) ).
code_byte( 187, byte(1,0,1,1,1,0,1,1) ).
code_byte( 188, byte(1,0,1,1,1,1,0,0) ).
code_byte( 189, byte(1,0,1,1,1,1,0,1) ).
code_byte( 190, byte(1,0,1,1,1,1,1,0) ).
code_byte( 191, byte(1,0,1,1,1,1,1,1) ).
code_byte( 192, byte(1,1,0,0,0,0,0,0) ).
code_byte( 193, byte(1,1,0,0,0,0,0,1) ).
code_byte( 194, byte(1,1,0,0,0,0,1,0) ).
code_byte( 195, byte(1,1,0,0,0,0,1,1) ).
code_byte( 196, byte(1,1,0,0,0,1,0,0) ).
code_byte( 197, byte(1,1,0,0,0,1,0,1) ).
code_byte( 198, byte(1,1,0,0,0,1,1,0) ).
code_byte( 199, byte(1,1,0,0,0,1,1,1) ).
code_byte( 200, byte(1,1,0,0,1,0,0,0) ).
code_byte( 201, byte(1,1,0,0,1,0,0,1) ).
code_byte( 202, byte(1,1,0,0,1,0,1,0) ).
code_byte( 203, byte(1,1,0,0,1,0,1,1) ).
code_byte( 204, byte(1,1,0,0,1,1,0,0) ).
code_byte( 205, byte(1,1,0,0,1,1,0,1) ).
code_byte( 206, byte(1,1,0,0,1,1,1,0) ).
code_byte( 207, byte(1,1,0,0,1,1,1,1) ).
code_byte( 208, byte(1,1,0,1,0,0,0,0) ).
code_byte( 209, byte(1,1,0,1,0,0,0,1) ).
code_byte( 210, byte(1,1,0,1,0,0,1,0) ).
code_byte( 211, byte(1,1,0,1,0,0,1,1) ).
code_byte( 212, byte(1,1,0,1,0,1,0,0) ).
code_byte( 213, byte(1,1,0,1,0,1,0,1) ).
code_byte( 214, byte(1,1,0,1,0,1,1,0) ).
code_byte( 215, byte(1,1,0,1,0,1,1,1) ).
code_byte( 216, byte(1,1,0,1,1,0,0,0) ).
code_byte( 217, byte(1,1,0,1,1,0,0,1) ).
code_byte( 218, byte(1,1,0,1,1,0,1,0) ).
code_byte( 219, byte(1,1,0,1,1,0,1,1) ).
code_byte( 220, byte(1,1,0,1,1,1,0,0) ).
code_byte( 221, byte(1,1,0,1,1,1,0,1) ).
code_byte( 222, byte(1,1,0,1,1,1,1,0) ).
code_byte( 223, byte(1,1,0,1,1,1,1,1) ).
code_byte( 224, byte(1,1,1,0,0,0,0,0) ).
code_byte( 225, byte(1,1,1,0,0,0,0,1) ).
code_byte( 226, byte(1,1,1,0,0,0,1,0) ).
code_byte( 227, byte(1,1,1,0,0,0,1,1) ).
code_byte( 228, byte(1,1,1,0,0,1,0,0) ).
code_byte( 229, byte(1,1,1,0,0,1,0,1) ).
code_byte( 230, byte(1,1,1,0,0,1,1,0) ).
code_byte( 231, byte(1,1,1,0,0,1,1,1) ).
code_byte( 232, byte(1,1,1,0,1,0,0,0) ).
code_byte( 233, byte(1,1,1,0,1,0,0,1) ).
code_byte( 234, byte(1,1,1,0,1,0,1,0) ).
code_byte( 235, byte(1,1,1,0,1,0,1,1) ).
code_byte( 236, byte(1,1,1,0,1,1,0,0) ).
code_byte( 237, byte(1,1,1,0,1,1,0,1) ).
code_byte( 238, byte(1,1,1,0,1,1,1,0) ).
code_byte( 239, byte(1,1,1,0,1,1,1,1) ).
code_byte( 240, byte(1,1,1,1,0,0,0,0) ).
code_byte( 241, byte(1,1,1,1,0,0,0,1) ).
code_byte( 242, byte(1,1,1,1,0,0,1,0) ).
code_byte( 243, byte(1,1,1,1,0,0,1,1) ).
code_byte( 244, byte(1,1,1,1,0,1,0,0) ).
code_byte( 245, byte(1,1,1,1,0,1,0,1) ).
code_byte( 246, byte(1,1,1,1,0,1,1,0) ).
code_byte( 247, byte(1,1,1,1,0,1,1,1) ).
code_byte( 248, byte(1,1,1,1,1,0,0,0) ).
code_byte( 249, byte(1,1,1,1,1,0,0,1) ).
code_byte( 250, byte(1,1,1,1,1,0,1,0) ).
code_byte( 251, byte(1,1,1,1,1,0,1,1) ).
code_byte( 252, byte(1,1,1,1,1,1,0,0) ).
code_byte( 253, byte(1,1,1,1,1,1,0,1) ).
code_byte( 254, byte(1,1,1,1,1,1,1,0) ).
code_byte( 255, byte(1,1,1,1,1,1,1,1) ).



code_b64([0,0,0,0,0,0], 'A' ).
code_b64([0,0,0,0,0,1], 'B' ).
code_b64([0,0,0,0,1,0], 'C' ).
code_b64([0,0,0,0,1,1], 'D' ).
code_b64([0,0,0,1,0,0], 'E' ).
code_b64([0,0,0,1,0,1], 'F' ).
code_b64([0,0,0,1,1,0], 'G' ).
code_b64([0,0,0,1,1,1], 'H' ).
code_b64([0,0,1,0,0,0], 'I' ).
code_b64([0,0,1,0,0,1], 'J' ).
code_b64([0,0,1,0,1,0], 'K' ).
code_b64([0,0,1,0,1,1], 'L' ).
code_b64([0,0,1,1,0,0], 'M' ).
code_b64([0,0,1,1,0,1], 'N' ).
code_b64([0,0,1,1,1,0], 'O' ).
code_b64([0,0,1,1,1,1], 'P' ).
code_b64([0,1,0,0,0,0], 'Q' ).
code_b64([0,1,0,0,0,1], 'R' ).
code_b64([0,1,0,0,1,0], 'S' ).
code_b64([0,1,0,0,1,1], 'T' ).
code_b64([0,1,0,1,0,0], 'U' ).
code_b64([0,1,0,1,0,1], 'V' ).
code_b64([0,1,0,1,1,0], 'W' ).
code_b64([0,1,0,1,1,1], 'X' ).
code_b64([0,1,1,0,0,0], 'Y' ).
code_b64([0,1,1,0,0,1], 'Z' ).
code_b64([0,1,1,0,1,0], 'a' ).
code_b64([0,1,1,0,1,1], 'b' ).
code_b64([0,1,1,1,0,0], 'c' ).
code_b64([0,1,1,1,0,1], 'd' ).
code_b64([0,1,1,1,1,0], 'e' ).
code_b64([0,1,1,1,1,1], 'f' ).
code_b64([1,0,0,0,0,0], 'g' ).
code_b64([1,0,0,0,0,1], 'h' ).
code_b64([1,0,0,0,1,0], 'i' ).
code_b64([1,0,0,0,1,1], 'j' ).
code_b64([1,0,0,1,0,0], 'k' ).
code_b64([1,0,0,1,0,1], 'l' ).
code_b64([1,0,0,1,1,0], 'm' ).
code_b64([1,0,0,1,1,1], 'n' ).
code_b64([1,0,1,0,0,0], 'o' ).
code_b64([1,0,1,0,0,1], 'p' ).
code_b64([1,0,1,0,1,0], 'q' ).
code_b64([1,0,1,0,1,1], 'r' ).
code_b64([1,0,1,1,0,0], 's' ).
code_b64([1,0,1,1,0,1], 't' ).
code_b64([1,0,1,1,1,0], 'u' ).
code_b64([1,0,1,1,1,1], 'v' ).
code_b64([1,1,0,0,0,0], 'w' ).
code_b64([1,1,0,0,0,1], 'x' ).
code_b64([1,1,0,0,1,0], 'y' ).
code_b64([1,1,0,0,1,1], 'z' ).
code_b64([1,1,0,1,0,0], '0' ).
code_b64([1,1,0,1,0,1], '1' ).
code_b64([1,1,0,1,1,0], '2' ).
code_b64([1,1,0,1,1,1], '3' ).
code_b64([1,1,1,0,0,0], '4' ).
code_b64([1,1,1,0,0,1], '5' ).
code_b64([1,1,1,0,1,0], '6' ).
code_b64([1,1,1,0,1,1], '7' ).
code_b64([1,1,1,1,0,0], '8' ).
code_b64([1,1,1,1,0,1], '9' ).
code_b64([1,1,1,1,1,0], '+' ).
code_b64([1,1,1,1,1,1], '/' ).
