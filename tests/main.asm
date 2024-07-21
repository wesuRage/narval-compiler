format ELF64 executable
entry main

include "/root/rust/narval/libs/x86_64/standard.s"

segment readable writeable
	gozadas db ArrayExpr(ArrayExpr { kind: ArrayExpr, elements: [NumericLiteral(NumericLiteral { kind: NumericLiteral, value: "1", typ: None, column: (28, 29), position: (27, 28), lineno: 1 }), NumericLiteral(NumericLiteral { kind: NumericLiteral, value: "2", typ: None, column: (31, 32), position: (30, 31), lineno: 1 }), NumericLiteral(NumericLiteral { kind: NumericLiteral, value: "3", typ: None, column: (34, 35), position: (33, 34), lineno: 1 }), NumericLiteral(NumericLiteral { kind: NumericLiteral, value: "4", typ: None, column: (37, 38), position: (36, 37), lineno: 1 }), NumericLiteral(NumericLiteral { kind: NumericLiteral, value: "5", typ: None, column: (40, 41), position: (39, 40), lineno: 1 }), NumericLiteral(NumericLiteral { kind: NumericLiteral, value: "5", typ: None, column: (43, 44), position: (42, 43), lineno: 1 }), NumericLiteral(NumericLiteral { kind: NumericLiteral, value: "6", typ: None, column: (46, 47), position: (45, 46), lineno: 1 }), NumericLiteral(NumericLiteral { kind: NumericLiteral, value: "7", typ: None, column: (49, 50), position: (48, 49), lineno: 1 }), NumericLiteral(NumericLiteral { kind: NumericLiteral, value: "8", typ: None, column: (52, 53), position: (51, 52), lineno: 1 }), NumericLiteral(NumericLiteral { kind: NumericLiteral, value: "9", typ: None, column: (55, 56), position: (54, 55), lineno: 1 }), NumericLiteral(NumericLiteral { kind: NumericLiteral, value: "10", typ: None, column: (58, 60), position: (57, 59), lineno: 1 })], typ: None, column: (27, 61), position: (26, 60), lineno: 1 })

segment readable executable
main:
	push 0
	call exit

