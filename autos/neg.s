	.meta source "\"autos/neg.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 13, 6
	seti r14, #0
	sub r15, r14, r15
	set r5, r15
	seti r12, #2
	set r6, r12
	set r7, r5
	set r8, r5
	add r9, r7, r8
	seti r10, #0
	sub r11, r10, r11
	invoke 4, 11, 0
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
