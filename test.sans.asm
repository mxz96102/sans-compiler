BITS 64
section .data
	a	DQ	0
	b	DQ	0
	d	DQ	0
	c	DQ	0
section .text	
global mystart
mystart:
	MOV	rax, 1
	MOV	[a], rax
	MOV	rax, 1073741824
	MOV	[b], rax
	MOV	rax, [a]
	MOV	rdx, 1
	IMUL	rdx, rbx
	SUB	rbx, rdx
	JE	IFYlG
	JMP	IFxbW
IFYlG: 
	MOV	rax, 1
	MOV	[c], rax
	MOV	rax, 1120403456
	MOV	rdx, [b]
	IMUL	rdx, rbx
	SUB	rbx, rdx
	JNS	WHXi1
WHXi1: 
	MOV	rax, [c]
	MOV	rdx, 2
	ADD	rdx, rbx
	MOV	rbx, rdx
	MOV	rax, [c]
	MOV	rdx, rbx
	IMUL	rdx, rbx
	SUB	rbx, rdx
	MOV	rax, 1120403456
	MOV	rdx, [b]
	IMUL	rdx, rbx
	SUB	rbx, rdx
	JNS	WHXi1
IFxbW: 
	MOV	rax, [a]
	MOV	rdx, 1
	IMUL	rdx, rbx
	SUB	rbx, rdx
	JNS	IFQCd
	JMP	IFQd3
IFQCd: 
	MOV	rax, 2
	MOV	[d], rax
	MOV	rax, 4
	MOV	rdx, 5
	IMUL	rdx, rbx
	MOV	rax, rbx
	MOV	rdx, 3
	ADD	rdx, rbx
	MOV	rbx, rdx
	MOV	rax, [d]
	MOV	rdx, rbx
	IMUL	rdx, rbx
	SUB	rbx, rdx
IFQd3: 
	push dword 0
	mov eax, 0x1
	sub esp, 4   
	INT	0x80

