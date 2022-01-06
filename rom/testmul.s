.equ LED_ADDR,      0x40000000
        li t3, LED_ADDR
        li a0, 3
        li t0, 1

out:    mul t0, t0, a0
        mul t0, a0, t0

        sb t0, 0(t3)

        li t1, 1000000
loop:   addi t1, t1, -1
        beqz t1, out
        j loop
