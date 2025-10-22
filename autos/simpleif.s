	.meta source "\"autos/simpleif.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 18, 6
	set r5, r18
	invoke 5, 17, 2
	set r6, r17
	set r14, r5
	set r15, r6
	goto_eq L8, r14, r15
	goto L9
L8:
	seti r16, #1
	invoke 4, 16, 0
	goto L10
L9:
L10:
	set r7, r5
	set r8, r6
	add r9, r7, r8
	seti r10, #2
	goto_ne L2, r9, r10
	goto L3
L2:
	invoke 5, 11, 3
	invoke 5, 12, 5
	goto_lt L5, r11, r12
	goto L6
L5:
	seti r13, #2
	invoke 4, 13, 0
	goto L7
L6:
L7:
	goto L4
L3:
L4:
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
