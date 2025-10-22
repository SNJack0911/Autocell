	.meta source "\"autos/addsub.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 9 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 26, 6
	seti r27, #1
	add r28, r26, r27
	set r5, r28
	set r23, r5
	seti r24, #1
	sub r25, r23, r24
	invoke 4, 25, 0
	seti r18, #5
	set r19, r5
	sub r20, r18, r19
	invoke 5, 21, 6
	sub r22, r20, r21
	set r6, r22
	seti r13, #5
	set r14, r5
	add r15, r13, r14
	invoke 5, 16, 6
	sub r17, r15, r16
	set r7, r17
	seti r8, #5
	set r9, r5
	sub r10, r8, r9
	invoke 5, 11, 6
	add r12, r10, r11
	invoke 4, 12, 0
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
