.equ LED_ADDR,      0x40000000
        li t3, LED_ADDR
        li a0, 2
        li t0, 256
	li t1, 0
out:    divu t0, t0, a0
        addi t1, t1, 1
        addi t1, t1, 2
        addi t1, t1, 3
        addi t1, t1, 4
        addi t1, t1, 5
        addi t1, t1, 6
        addi t1, t1, 7
        addi t1, t1, 8
        addi t1, t1, 9
        addi t1, t1, 10
	sb t1, 0(t3)
        addi t1, t1, 11
        addi t1, t1, 12
        addi t1, t1, 13
	addi t1, t1, 14
	addi t1, t1, 15
	addi t1, t1, 16
	sb t1, 0(t3)
        nop
        nop
        nop
        divu t1, t1, a0
        sb t0, 0(t3)

end:	j end
