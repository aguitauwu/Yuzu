@ ============================================================
@ Yuzu TUI Monitor — ARM32 Android (Bionic/Termux)
@ Compilar:
@   as -march=armv7-a -o yuzu-tui-arm32-android.o yuzu-tui-arm32-android.s
@   ld --pie -dynamic-linker /system/bin/linker \
@      -rpath /data/data/com.termux/files/usr/lib \
@      -L /data/data/com.termux/files/usr/lib \
@      -o yuzu-tui-arm32-android yuzu-tui-arm32-android.o -lc
@ ============================================================
@ DIFERENCIA CLAVE vs ARM32 Linux:
@   - ai_addr offset en Bionic ARM32: 20 (igual que glibc en este caso)
@   - linker: /system/bin/linker en lugar de /lib/ld-linux-armhf.so.3
@   - --pie obligatorio
@ ============================================================

.extern getaddrinfo
.extern freeaddrinfo
.extern usleep

.section .data

banner:
.ascii "\033[2J\033[H"
.ascii "\033[38;2;244;114;182m\033[1m"
.ascii "  +-----------------------------------------+\n"
.ascii "  |                                         |\n"
.ascii "  |   * Y U Z U  T U I  ARM32 Android       |\n"
.ascii "  |   API Monitor -- Bionic/Termux           |\n"
.ascii "  |                                         |\n"
.ascii "  +-----------------------------------------+\n"
.ascii "\033[0m\n"
banner_len = . - banner

lbl_status: .ascii "\033[38;2;244;114;182m  * STATUS   \033[0m\033[38;2;250;250;250m"
lbl_status_len = . - lbl_status
lbl_total:  .ascii "\033[38;2;244;114;182m  * REQUESTS \033[0m\033[38;2;250;250;250m"
lbl_total_len = . - lbl_total
lbl_ok:     .ascii "\033[38;2;134;239;172m  * OK       \033[0m\033[38;2;250;250;250m"
lbl_ok_len = . - lbl_ok
lbl_err:    .ascii "\033[38;2;248;113;113m  * ERRORS   \033[0m\033[38;2;250;250;250m"
lbl_err_len = . - lbl_err
lbl_avg:    .ascii "\033[38;2;251;191;36m  * AVG LAT  \033[0m\033[38;2;250;250;250m"
lbl_avg_len = . - lbl_avg
lbl_uptime: .ascii "\033[38;2;244;114;182m  * UPTIME   \033[0m\033[38;2;250;250;250m"
lbl_uptime_len = . - lbl_uptime

str_ok:     .ascii "\033[38;2;134;239;172m ONLINE \033[0m\n"
str_ok_len = . - str_ok
str_offline:.ascii "\033[38;2;248;113;113m OFFLINE\033[0m\n"
str_offline_len = . - str_offline
str_ms:     .ascii "ms\033[0m\n"
str_ms_len = . - str_ms
str_s:      .ascii "s\033[0m\n"
str_s_len = . - str_s
str_newline:.ascii "\n"
str_sep:    .ascii "\033[38;2;60;60;60m  --------------------------------------------\033[0m\n"
str_sep_len = . - str_sep
str_home:   .ascii "\033[6;1H"
str_home_len = . - str_home

api_host:   .asciz "127.0.0.1"
api_port:   .asciz "8080"
http_req:   .ascii "GET /metrics HTTP/1.1\r\nHost: 127.0.0.1:8080\r\nConnection: close\r\n\r\n"
http_req_len = . - http_req

key_total:  .asciz "total_requests\":"
key_avg:    .asciz "avg_latency_ms\":"
key_uptime: .asciz "uptime_seconds\":"

.section .bss
.align 4
resp_buf:   .space 8192
hints_buf:  .space 32
addrinfo_p: .space 4
sock_store: .space 4

@ ARM32 syscalls
.equ SYS_read,    3
.equ SYS_write,   4
.equ SYS_close,   6
.equ SYS_socket,  281
.equ SYS_connect, 283
.equ SYS_exit,    1
.equ AF_INET,     2
.equ SOCK_STREAM, 1

@ addrinfo offsets Bionic ARM32 (iguales a glibc en ARM32)
.equ AI_FAMILY,   4
.equ AI_SOCKTYPE, 8
.equ AI_ADDR,     20
.equ AI_ADDRLEN,  16

.section .text
.global _start

_start:
    mov r0, #1
    ldr r1, =banner
    mov r2, #banner_len
    mov r7, #SYS_write
    svc #0

monitor_loop:
    mov r0, #1
    ldr r1, =str_home
    mov r2, #str_home_len
    mov r7, #SYS_write
    svc #0

    bl do_http_request
    cmp r0, #0
    blt show_offline

    bl display_metrics
    b sleep_loop

show_offline:
    mov r0, #1
    ldr r1, =lbl_status
    mov r2, #lbl_status_len
    mov r7, #SYS_write
    svc #0
    mov r0, #1
    ldr r1, =str_offline
    mov r2, #str_offline_len
    mov r7, #SYS_write
    svc #0

sleep_loop:
    mov r0, #2000000
    bl usleep
    b monitor_loop

do_http_request:
    push {r4-r7, lr}

    ldr r0, =hints_buf
    mov r1, #0
    mov r2, #32
    bl memset_arm32

    ldr r4, =hints_buf
    mov r1, #AF_INET
    str r1, [r4, #AI_FAMILY]
    mov r1, #SOCK_STREAM
    str r1, [r4, #AI_SOCKTYPE]

    ldr r0, =api_host
    ldr r1, =api_port
    mov r2, r4
    ldr r3, =addrinfo_p
    bl getaddrinfo
    cmp r0, #0
    bne .dhr_fail

    ldr r4, =addrinfo_p
    ldr r4, [r4]
    ldr r5, [r4, #AI_ADDR]
    ldr r6, [r4, #AI_ADDRLEN]

    mov r0, #AF_INET
    mov r1, #SOCK_STREAM
    mov r2, #0
    mov r7, #SYS_socket
    svc #0
    cmp r0, #0
    blt .dhr_fail
    mov r4, r0
    ldr r1, =sock_store
    str r4, [r1]

    mov r0, r4
    mov r1, r5
    mov r2, r6
    mov r7, #SYS_connect
    svc #0
    cmp r0, #0
    blt .dhr_fail

    ldr r0, =addrinfo_p
    ldr r0, [r0]
    bl freeaddrinfo

    mov r0, r4
    ldr r1, =http_req
    mov r2, #http_req_len
    mov r7, #SYS_write
    svc #0

    mov r0, r4
    ldr r1, =resp_buf
    mov r2, #8191
    mov r7, #SYS_read
    svc #0
    mov r5, r0

    mov r0, r4
    mov r7, #SYS_close
    svc #0

    mov r0, r5
    b .dhr_done

.dhr_fail:
    mov r0, #-1
.dhr_done:
    pop {r4-r7, pc}

display_metrics:
    push {r4-r5, lr}

    mov r0, #1
    ldr r1, =lbl_status
    mov r2, #lbl_status_len
    mov r7, #SYS_write
    svc #0
    mov r0, #1
    ldr r1, =str_ok
    mov r2, #str_ok_len
    mov r7, #SYS_write
    svc #0
    bl print_sep_arm32

    ldr r0, =key_total
    bl find_val_arm32
    cmp r0, #0
    beq .dma_t
    mov r4, r0
    mov r0, #1
    ldr r1, =lbl_total
    mov r2, #lbl_total_len
    mov r7, #SYS_write
    svc #0
    mov r0, r4
    bl print_delim_arm32
    mov r0, #1
    ldr r1, =str_newline
    mov r2, #1
    mov r7, #SYS_write
    svc #0
.dma_t:

    ldr r0, =key_avg
    bl find_val_arm32
    cmp r0, #0
    beq .dma_avg
    mov r4, r0
    mov r0, #1
    ldr r1, =lbl_avg
    mov r2, #lbl_avg_len
    mov r7, #SYS_write
    svc #0
    mov r0, r4
    bl print_delim_arm32
    mov r0, #1
    ldr r1, =str_ms
    mov r2, #str_ms_len
    mov r7, #SYS_write
    svc #0
.dma_avg:

    ldr r0, =key_uptime
    bl find_val_arm32
    cmp r0, #0
    beq .dma_up
    mov r4, r0
    mov r0, #1
    ldr r1, =lbl_uptime
    mov r2, #lbl_uptime_len
    mov r7, #SYS_write
    svc #0
    mov r0, r4
    bl print_delim_arm32
    mov r0, #1
    ldr r1, =str_s
    mov r2, #str_s_len
    mov r7, #SYS_write
    svc #0
.dma_up:

    bl print_sep_arm32
    pop {r4-r5, pc}

find_val_arm32:
    push {r4-r6, lr}
    mov r4, r0
    ldr r5, =resp_buf
    mov r6, #8192
.fva_loop:
    cmp r6, #0
    ble .fva_miss
    mov r0, r5
    mov r1, r4
    bl pfx_match_arm32
    cmp r0, #0
    beq .fva_hit
    add r5, r5, #1
    sub r6, r6, #1
    b .fva_loop
.fva_hit:
    mov r0, r4
    bl strlen_arm32
    add r5, r5, r0
1:  ldrb r1, [r5]
    cmp r1, #' '
    bne 2f
    add r5, r5, #1
    b 1b
2:  mov r0, r5
    b .fva_done
.fva_miss:
    mov r0, #0
.fva_done:
    pop {r4-r6, pc}

print_delim_arm32:
    push {r4-r5, lr}
    mov r4, r0
    mov r5, #0
1:  ldrb r1, [r4, r5]
    cmp r1, #','
    beq 2f
    cmp r1, #'}'
    beq 2f
    cmp r1, #'\r'
    beq 2f
    cmp r1, #0
    beq 2f
    add r5, r5, #1
    b 1b
2:  mov r0, #1
    mov r1, r4
    mov r2, r5
    mov r7, #SYS_write
    svc #0
    pop {r4-r5, pc}

print_sep_arm32:
    mov r0, #1
    ldr r1, =str_sep
    mov r2, #str_sep_len
    mov r7, #SYS_write
    svc #0
    bx lr

pfx_match_arm32:
1:  ldrb r2, [r1]
    cmp r2, #0
    beq .pfx_ok
    ldrb r3, [r0]
    cmp r2, r3
    bne .pfx_fail
    add r0, r0, #1
    add r1, r1, #1
    b 1b
.pfx_ok:
    mov r0, #0
    bx lr
.pfx_fail:
    mov r0, #1
    bx lr

strlen_arm32:
    mov r1, #0
1:  ldrb r2, [r0, r1]
    cmp r2, #0
    beq 2f
    add r1, r1, #1
    b 1b
2:  mov r0, r1
    bx lr

memset_arm32:
1:  cmp r2, #0
    beq 2f
    strb r1, [r0]
    add r0, r0, #1
    sub r2, r2, #1
    b 1b
2:  bx lr
