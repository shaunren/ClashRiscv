.equ LED_ADDR,      0x40000000
        li t3, LED_ADDR
        li t0, 0

out:    sb t0, 0(t3)
        li t1, 1000000
        
        addi t0, t0, 1
        andi t0, t0, 0xff

loop:   addi t1, t1, -1
        beqz t1, out
        j loop
