.equ LED_ADDR,      0x40000000
        #  Calculate fib(12)        
        li x1, 6
        sw x1, 4(x0)

        lbu a0, 4(x0)
        lbu t0, 4(x0)
        add a0, a0, t0

        li t3, LED_ADDR
        sb a0, 0(t3)

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
