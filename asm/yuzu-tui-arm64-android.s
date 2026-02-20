// ============================================================
// Yuzu TUI Monitor — ARM64 Android (Bionic/Termux)
// Compilar:
//   as -o yuzu-tui-arm64-android.o yuzu-tui-arm64-android.s
//   ld --pie -dynamic-linker /system/bin/linker64 \
//      -rpath /data/data/com.termux/files/usr/lib \
//      -L /data/data/com.termux/files/usr/lib \
//      -o yuzu-tui-arm64-android yuzu-tui-arm64-android.o -lc
// ============================================================

.extern getaddrinfo
.extern freeaddrinfo
.extern usleep

.section .data

banner:
.ascii "\033[2J\033[H"
.ascii "\033[38;2;244;114;182m\033[1m"
.ascii "  \xe2\x95\xad\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x95\xae\n"
.ascii "  \xe2\x94\x82                                         \xe2\x94\x82\n"
.ascii "  \xe2\x94\x82   \xe2\x9c\xa6  Y U Z U  T U I  ARM64 Android     \xe2\x94\x82\n"
.ascii "  \xe2\x94\x82   API Monitor \xe2\x80\x94 Bionic/Termux             \xe2\x94\x82\n"
.ascii "  \xe2\x94\x82                                         \xe2\x94\x82\n"
.ascii "  \xe2\x95\xb0\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x95\xaf\n"
.ascii "\033[0m\n"
banner_len = . - banner

lbl_status:     .ascii "\033[38;2;244;114;182m\033[1m  \xe2\x9c\xa6 STATUS   \033[0m\033[38;2;250;250;250m"
lbl_status_len = . - lbl_status
lbl_total:      .ascii "\033[38;2;244;114;182m\033[1m  \xe2\x9c\xa6 REQUESTS \033[0m\033[38;2;250;250;250m"
lbl_total_len = . - lbl_total
lbl_ok:         .ascii "\033[38;2;134;239;172m\033[1m  \xe2\x9c\xa6 OK       \033[0m\033[38;2;250;250;250m"
lbl_ok_len = . - lbl_ok
lbl_err:        .ascii "\033[38;2;248;113;113m\033[1m  \xe2\x9c\xa6 ERRORS   \033[0m\033[38;2;250;250;250m"
lbl_err_len = . - lbl_err
lbl_avg:        .ascii "\033[38;2;251;191;36m\033[1m  \xe2\x9c\xa6 AVG LAT  \033[0m\033[38;2;250;250;250m"
lbl_avg_len = . - lbl_avg
lbl_uptime:     .ascii "\033[38;2;244;114;182m\033[1m  \xe2\x9c\xa6 UPTIME   \033[0m\033[38;2;250;250;250m"
lbl_uptime_len = . - lbl_uptime

str_ok:         .ascii "\033[38;2;134;239;172m\033[1m ONLINE \033[0m\n"
str_ok_len = . - str_ok
str_offline:    .ascii "\033[38;2;248;113;113m\033[1m OFFLINE\033[0m\n"
str_offline_len = . - str_offline
str_ms:         .ascii "ms\033[0m\n"
str_ms_len = . - str_ms
str_s:          .ascii "s\033[0m\n"
str_s_len = . - str_s
str_newline:    .ascii "\n"
str_sep:        .ascii "\033[38;2;60;60;60m  \xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\033[0m\n"
str_sep_len = . - str_sep
str_home:       .ascii "\033[6;1H"
str_home_len = . - str_home

api_host:       .asciz "127.0.0.1"
api_port_str:   .asciz "8080"
http_req:       .ascii "GET /metrics HTTP/1.1\r\nHost: 127.0.0.1:8080\r\nConnection: close\r\n\r\n"
http_req_len = . - http_req

key_total:      .asciz "total_requests\":"
key_ok:         .asciz "ok_requests\":"
key_err:        .asciz "error_requests\":"
key_avg:        .asciz "avg_latency_ms\":"
key_uptime:     .asciz "uptime_seconds\":"

.section .bss
.align 8
resp_buf:       .space 8192
hints_buf:      .space 48
addrinfo_ptr:   .space 8
sock_fd_store:  .space 8

// ARM64 syscall numbers (mismos en Android)
.equ SYS_read,    63
.equ SYS_write,   64
.equ SYS_close,   57
.equ SYS_socket,  198
.equ SYS_connect, 203
.equ SYS_exit,    93
.equ AF_INET,     2
.equ SOCK_STREAM, 1

// DIFERENCIA CLAVE: Bionic usa offset 32 para ai_addr
// glibc usa offset 24
.equ AI_FAMILY,   4
.equ AI_SOCKTYPE, 8
.equ AI_ADDR,     32      // ← Bionic offset (glibc es 24)
.equ AI_ADDRLEN,  16

.section .text
.global _start

_start:
    mov x0, #1
    adr x1, banner
    mov x2, #banner_len
    mov x8, #SYS_write
    svc #0

monitor_loop:
    mov x0, #1
    adr x1, str_home
    mov x2, #str_home_len
    mov x8, #SYS_write
    svc #0

    bl do_http_request
    cmp x0, #0
    blt show_offline

    bl display_metrics
    b sleep_loop

show_offline:
    mov x0, #1
    adr x1, lbl_status
    mov x2, #lbl_status_len
    mov x8, #SYS_write
    svc #0
    mov x0, #1
    adr x1, str_offline
    mov x2, #str_offline_len
    mov x8, #SYS_write
    svc #0

sleep_loop:
    mov x0, #2000000
    bl usleep
    b monitor_loop

do_http_request:
    stp x29, x30, [sp, #-48]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]

    adr x0, hints_buf
    mov x1, #0
    mov x2, #48
    bl memset_simple

    adr x19, hints_buf
    mov w1, #AF_INET
    str w1, [x19, #AI_FAMILY]
    mov w1, #SOCK_STREAM
    str w1, [x19, #AI_SOCKTYPE]

    adr x0, api_host
    adr x1, api_port_str
    mov x2, x19
    adr x3, addrinfo_ptr
    bl getaddrinfo
    cbnz x0, .req_fail

    adr x19, addrinfo_ptr
    ldr x19, [x19]
    ldr x20, [x19, #AI_ADDR]   // offset 32 en Bionic
    ldr w21, [x19, #AI_ADDRLEN]

    mov x0, #AF_INET
    mov x1, #SOCK_STREAM
    mov x2, #0
    mov x8, #SYS_socket
    svc #0
    tst x0, x0
    bmi .req_fail
    mov x22, x0

    mov x0, x22
    mov x1, x20
    mov x2, x21
    mov x8, #SYS_connect
    svc #0
    tst x0, x0
    bmi .req_fail

    adr x0, addrinfo_ptr
    ldr x0, [x0]
    bl freeaddrinfo

    mov x0, x22
    adr x1, http_req
    mov x2, #http_req_len
    mov x8, #SYS_write
    svc #0

    mov x0, x22
    adr x1, resp_buf
    mov x2, #8191
    mov x8, #SYS_read
    svc #0
    mov x19, x0

    mov x0, x22
    mov x8, #SYS_close
    svc #0

    mov x0, x19
    b .req_done

.req_fail:
    mov x0, #-1

.req_done:
    ldp x19, x20, [sp, #16]
    ldp x21, x22, [sp, #32]
    ldp x29, x30, [sp, #-48]!
    add sp, sp, #48
    ret

display_metrics:
    stp x29, x30, [sp, #-32]!
    mov x29, sp
    stp x19, x20, [sp, #16]

    mov x0, #1
    adr x1, lbl_status
    mov x2, #lbl_status_len
    mov x8, #SYS_write
    svc #0
    mov x0, #1
    adr x1, str_ok
    mov x2, #str_ok_len
    mov x8, #SYS_write
    svc #0
    bl print_sep

    adr x0, key_total
    bl find_value
    cbz x0, .dm_t
    mov x19, x0
    mov x0, #1
    adr x1, lbl_total
    mov x2, #lbl_total_len
    mov x8, #SYS_write
    svc #0
    mov x0, x19
    bl print_until_delim
    mov x0, #1
    adr x1, str_newline
    mov x2, #1
    mov x8, #SYS_write
    svc #0
.dm_t:
    adr x0, key_ok
    bl find_value
    cbz x0, .dm_ok
    mov x19, x0
    mov x0, #1
    adr x1, lbl_ok
    mov x2, #lbl_ok_len
    mov x8, #SYS_write
    svc #0
    mov x0, x19
    bl print_until_delim
    mov x0, #1
    adr x1, str_newline
    mov x2, #1
    mov x8, #SYS_write
    svc #0
.dm_ok:
    adr x0, key_err
    bl find_value
    cbz x0, .dm_err
    mov x19, x0
    mov x0, #1
    adr x1, lbl_err
    mov x2, #lbl_err_len
    mov x8, #SYS_write
    svc #0
    mov x0, x19
    bl print_until_delim
    mov x0, #1
    adr x1, str_newline
    mov x2, #1
    mov x8, #SYS_write
    svc #0
.dm_err:
    adr x0, key_avg
    bl find_value
    cbz x0, .dm_avg
    mov x19, x0
    mov x0, #1
    adr x1, lbl_avg
    mov x2, #lbl_avg_len
    mov x8, #SYS_write
    svc #0
    mov x0, x19
    bl print_until_delim
    mov x0, #1
    adr x1, str_ms
    mov x2, #str_ms_len
    mov x8, #SYS_write
    svc #0
.dm_avg:
    adr x0, key_uptime
    bl find_value
    cbz x0, .dm_up
    mov x19, x0
    mov x0, #1
    adr x1, lbl_uptime
    mov x2, #lbl_uptime_len
    mov x8, #SYS_write
    svc #0
    mov x0, x19
    bl print_until_delim
    mov x0, #1
    adr x1, str_s
    mov x2, #str_s_len
    mov x8, #SYS_write
    svc #0
.dm_up:
    bl print_sep

    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp, #-32]!
    add sp, sp, #32
    ret

find_value:
    stp x29, x30, [sp, #-32]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    mov x19, x0
    adr x20, resp_buf
    mov x2, #8192
.fv_loop:
    cbz x2, .fv_miss
    mov x0, x20
    mov x1, x19
    bl strcmp_prefix
    cbz x0, .fv_hit
    add x20, x20, #1
    sub x2, x2, #1
    b .fv_loop
.fv_hit:
    mov x0, x19
    bl strlen_x0
    add x20, x20, x0
1:  ldrb w1, [x20]
    cmp w1, #' '
    bne 2f
    add x20, x20, #1
    b 1b
2:  mov x0, x20
    b .fv_done
.fv_miss:
    mov x0, #0
.fv_done:
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp, #-32]!
    add sp, sp, #32
    ret

print_until_delim:
    mov x19, x0
    mov x20, #0
1:  ldrb w1, [x19, x20]
    cmp w1, #','
    beq 2f
    cmp w1, #'}'
    beq 2f
    cmp w1, #'\r'
    beq 2f
    cmp w1, #'\n'
    beq 2f
    cbz w1, 2f
    add x20, x20, #1
    b 1b
2:  mov x0, #1
    mov x1, x19
    mov x2, x20
    mov x8, #SYS_write
    svc #0
    ret

print_sep:
    mov x0, #1
    adr x1, str_sep
    mov x2, #str_sep_len
    mov x8, #SYS_write
    svc #0
    ret

strcmp_prefix:
1:  ldrb w2, [x1]
    cbz w2, .spm
    ldrb w3, [x0]
    cmp w2, w3
    bne .spx
    add x0, x0, #1
    add x1, x1, #1
    b 1b
.spm: mov x0, #0
    ret
.spx: mov x0, #1
    ret

strlen_x0:
    mov x1, #0
1:  ldrb w2, [x0, x1]
    cbz w2, 2f
    add x1, x1, #1
    b 1b
2:  mov x0, x1
    ret

memset_simple:
1:  cbz x2, 2f
    strb w1, [x0]
    add x0, x0, #1
    sub x2, x2, #1
    b 1b
2:  ret
