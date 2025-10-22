	.meta source "\"autos/mult.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 9 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 29, 0
	seti r30, #3
	mul r31, r29, r30
	set r5, r31
	seti r24, #1
	invoke 5, 25, 1
	mul r26, r24, r25
	set r27, r5
	mul r28, r26, r27
	invoke 4, 28, 0
	seti r19, #1
	set r20, r5
	invoke 5, 21, 0
	mul r22, r20, r21
	add r23, r19, r22
	set r6, r23
	set r14, r6
	invoke 5, 15, 0
	mul r16, r14, r15
	seti r17, #1
	add r18, r16, r17
	set r7, r18
	seti r9, #1
	set r10, r7
	add r11, r9, r10
	invoke 5, 12, 0
	mul r13, r11, r12
	set r8, r13
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
