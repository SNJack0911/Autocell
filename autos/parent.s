	.meta source "\"autos/parent.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 9 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 30, 0
	invoke 5, 31, 6
	add r32, r30, r31
	seti r33, #5
	add r34, r32, r33
	set r5, r34
	invoke 5, 25, 0
	invoke 5, 26, 6
	seti r27, #5
	add r28, r26, r27
	add r29, r25, r28
	set r6, r29
	invoke 5, 20, 0
	invoke 5, 21, 6
	seti r22, #5
	add r23, r21, r22
	add r24, r20, r23
	set r7, r24
	invoke 5, 15, 0
	invoke 5, 16, 6
	seti r17, #1
	add r18, r16, r17
	sub r19, r15, r18
	set r8, r19
	invoke 5, 10, 0
	invoke 5, 11, 6
	seti r12, #1
	sub r13, r11, r12
	sub r14, r10, r13
	set r9, r14
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
