.equ LED_ADDR,      0x40000000
#  Calculate fib(12)
        li a0, 10
        addi a0, a0, 2

        addi t3, a0, -1

        li t0, 0
        li t1, 1

loop:   add t2, t0, t1
        mv t0, t1
        mv t1, t2

        addi t3, t3, -1
        bnez t3, loop


	sw t1, 0(x0)
        lw x1, 0(x0)

        li t3, LED_ADDR
        sb t1, 0(t3)   # output result to MMIO LEDs

end:    j end
