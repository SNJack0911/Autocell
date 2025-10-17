	.meta source "\"autos/divmod.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 9 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 14, 0
	set r15, r12
	sub r16, r14, r15
	invoke 5, 17, 6
	mul r18, r16, r17
	seti r19, #2
	div r20, r18, r19
	set r13, r20
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
