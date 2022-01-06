.equ LED_ADDR,      0x40000000
        li t2, 256
        li t3, LED_ADDR
        li t0, 1

out:    sb t0, 0(t3)
        li t1, 3000000
        
        slli t0, t0, 1
        blt t0, t2, loop
        li t0, 1

loop:   addi t1, t1, -1
        beqz t1, out
        j loop
