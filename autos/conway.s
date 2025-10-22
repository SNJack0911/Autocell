	.meta source "\"autos/conway.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 17, 2
	invoke 5, 18, 1
	add r19, r17, r18
	invoke 5, 20, 8
	add r21, r19, r20
	invoke 5, 22, 7
	add r23, r21, r22
	invoke 5, 24, 6
	add r25, r23, r24
	invoke 5, 26, 5
	add r27, r25, r26
	invoke 5, 28, 4
	add r29, r27, r28
	invoke 5, 30, 3
	add r31, r29, r30
	set r5, r31
	invoke 5, 6, 0
	seti r7, #1
	goto_eq L2, r6, r7
	goto L3
L2:
	set r8, r5
	seti r9, #2
	goto_lt L5, r8, r9
	goto L6
L5:
	seti r10, #0
	invoke 4, 10, 0
	goto L7
L6:
	set r11, r5
	seti r12, #3
	goto_gt L8, r11, r12
	goto L9
L8:
	seti r13, #0
	invoke 4, 13, 0
	goto L10
L9:
L10:
L7:
	goto L4
L3:
	set r14, r5
	seti r15, #3
	goto_eq L11, r14, r15
	goto L12
L11:
	seti r16, #1
	invoke 4, 16, 0
	goto L13
L12:
L13:
L4:
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
