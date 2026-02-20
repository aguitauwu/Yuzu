// ============================================================
// Yuzu TUI Monitor — x86_64 Linux
// Compilar:
//   as -o yuzu-tui-x86_64.o yuzu-tui-x86_64.s
//   ld -dynamic-linker /lib64/ld-linux-x86-64.so.2 \
//      -o yuzu-tui-x86_64 yuzu-tui-x86_64.o -lc
// ============================================================

.extern getaddrinfo
.extern freeaddrinfo
.extern usleep
.extern atoi

.section .data

// Banner
banner:
.ascii "\033[2J\033[H"
.ascii "\033[38;2;244;114;182m\033[1m"
.ascii "  \xe2\x95\xad\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x95\xae\n"
.ascii "  \xe2\x94\x82                                         \xe2\x94\x82\n"
.ascii "  \xe2\x94\x82   \xe2\x9c\xa6  Y U Z U  T U I  x86_64             \xe2\x94\x82\n"
.ascii "  \xe2\x94\x82   API Monitor \xe2\x80\x94 Built in Assembly         \xe2\x94\x82\n"
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
lbl_last:       .ascii "\033[38;2;251;191;36m\033[1m  \xe2\x9c\xa6 LAST LAT \033[0m\033[38;2;250;250;250m"
lbl_last_len = . - lbl_last
lbl_uptime:     .ascii "\033[38;2;244;114;182m\033[1m  \xe2\x9c\xa6 UPTIME   \033[0m\033[38;2;250;250;250m"
lbl_uptime_len = . - lbl_uptime

str_ok:     .ascii "\033[38;2;134;239;172m\033[1m ONLINE \033[0m"
str_ok_len = . - str_ok
str_err:    .ascii "\033[38;2;248;113;113m\033[1m OFFLINE\033[0m"
str_err_len = . - str_err
str_ms:     .ascii "ms\033[0m"
str_ms_len = . - str_ms
str_s:      .ascii "s\033[0m"
str_s_len = . - str_s
str_newline: .ascii "\n"
str_sep:    .ascii "\033[38;2;60;60;60m  \xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\033[0m\n"
str_sep_len = . - str_sep
str_cursor_home: .ascii "\033[H"
str_cursor_home_len = . - str_cursor_home
str_bar_fill:   .ascii "\xe2\x96\x88"
str_bar_empty:  .ascii "\xe2\x96\x91"

api_host:       .asciz "127.0.0.1"
api_port_str:   .asciz "8080"
http_req:       .ascii "GET /metrics HTTP/1.1\r\nHost: 127.0.0.1:8080\r\nConnection: close\r\n\r\n"
http_req_len = . - http_req

key_total:      .asciz "total_requests\":"
key_ok:         .asciz "ok_requests\":"
key_err:        .asciz "error_requests\":"
key_avg:        .asciz "avg_latency_ms\":"
key_last:       .asciz "last_latency_ms\":"
key_uptime:     .asciz "uptime_seconds\":"

.section .bss
.align 8
resp_buf:       .space 8192
num_buf:        .space 32
hints_buf:      .space 48
addrinfo_ptr:   .space 8
sock_fd:        .space 8

.equ AI_FAMILY,   4
.equ AI_SOCKTYPE, 8
.equ AI_ADDR,     24
.equ AI_ADDRLEN,  16

.equ SYS_read,    0
.equ SYS_write,   1
.equ SYS_close,   3
.equ SYS_socket,  41
.equ SYS_connect, 42
.equ SYS_exit,    60
.equ AF_INET,     2
.equ SOCK_STREAM, 1

.section .text
.global _start

// ============================================================
_start:
    // Mostrar banner
    mov $1, %rdi
    lea banner(%rip), %rsi
    mov $banner_len, %rdx
    mov $SYS_write, %rax
    syscall

monitor_loop:
    // Volver al inicio del área de datos (debajo del banner)
    mov $1, %rdi
    lea str_cursor_home(%rip), %rsi
    mov $str_cursor_home_len, %rdx
    mov $SYS_write, %rax
    syscall

    // Hacer HTTP request
    call do_http_request

    // Si falla, mostrar OFFLINE
    cmp $0, %rax
    jl show_offline

    // Parsear y mostrar métricas
    call display_metrics
    jmp sleep_and_loop

show_offline:
    mov $1, %rdi
    lea lbl_status(%rip), %rsi
    mov $lbl_status_len, %rdx
    mov $SYS_write, %rax
    syscall
    mov $1, %rdi
    lea str_err(%rip), %rsi
    mov $str_err_len, %rdx
    mov $SYS_write, %rax
    syscall
    mov $1, %rdi
    lea str_newline(%rip), %rsi
    mov $1, %rdx
    mov $SYS_write, %rax
    syscall

sleep_and_loop:
    // usleep(2000000) = 2 segundos
    mov $2000000, %rdi
    call usleep
    jmp monitor_loop

// ============================================================
// do_http_request — TCP socket + send + recv
// ============================================================
do_http_request:
    push %rbp
    mov %rsp, %rbp
    push %r12

    // limpiar hints
    lea hints_buf(%rip), %rdi
    xor %eax, %eax
    mov $48, %rcx
    rep stosb

    lea hints_buf(%rip), %rbx
    movl $AF_INET, AI_FAMILY(%rbx)
    movl $SOCK_STREAM, AI_SOCKTYPE(%rbx)

    lea api_host(%rip), %rdi
    lea api_port_str(%rip), %rsi
    lea hints_buf(%rip), %rdx
    lea addrinfo_ptr(%rip), %rcx
    call getaddrinfo
    test %eax, %eax
    jnz .req_fail

    mov addrinfo_ptr(%rip), %rbx
    mov AI_ADDR(%rbx), %r12
    movl AI_ADDRLEN(%rbx), %r13d

    mov $AF_INET, %rdi
    mov $SOCK_STREAM, %rsi
    xor %rdx, %rdx
    mov $SYS_socket, %rax
    syscall
    test %rax, %rax
    js .req_fail
    mov %rax, sock_fd(%rip)
    mov %rax, %r14

    mov %r14, %rdi
    mov %r12, %rsi
    movslq %r13d, %rdx
    mov $SYS_connect, %rax
    syscall
    test %rax, %rax
    js .req_fail

    mov addrinfo_ptr(%rip), %rdi
    call freeaddrinfo

    // enviar request
    mov %r14, %rdi
    lea http_req(%rip), %rsi
    mov $http_req_len, %rdx
    mov $1, %rax           // SYS_write
    syscall

    // leer response
    mov %r14, %rdi
    lea resp_buf(%rip), %rsi
    mov $8191, %rdx
    mov $SYS_read, %rax
    syscall
    mov %rax, %r15    // bytes leídos

    mov %r14, %rdi
    mov $SYS_close, %rax
    syscall

    mov %r15, %rax
    pop %r12
    pop %rbp
    ret

.req_fail:
    mov $-1, %rax
    pop %r12
    pop %rbp
    ret

// ============================================================
// display_metrics — parsear JSON y mostrar en TUI
// ============================================================
display_metrics:
    push %rbp
    mov %rsp, %rbp

    // STATUS: ONLINE
    mov $1, %rdi
    lea lbl_status(%rip), %rsi
    mov $lbl_status_len, %rdx
    mov $SYS_write, %rax
    syscall
    mov $1, %rdi
    lea str_ok(%rip), %rsi
    mov $str_ok_len, %rdx
    mov $SYS_write, %rax
    syscall
    mov $1, %rdi
    lea str_newline(%rip), %rsi
    mov $1, %rdx
    mov $SYS_write, %rax
    syscall

    // separador
    call print_sep

    // total_requests
    lea key_total(%rip), %rdi
    call find_value
    test %rax, %rax
    jz .skip_total
    mov %rax, %rsi
    mov $1, %rdi
    lea lbl_total(%rip), %rcx
    push %rsi
    mov %rcx, %rsi
    mov $lbl_total_len, %rdx
    mov $SYS_write, %rax
    syscall
    pop %rsi
    call print_number_str

.skip_total:
    // ok_requests
    lea key_ok(%rip), %rdi
    call find_value
    test %rax, %rax
    jz .skip_ok
    mov %rax, %rsi
    push %rsi
    mov $1, %rdi
    lea lbl_ok(%rip), %rsi
    mov $lbl_ok_len, %rdx
    mov $SYS_write, %rax
    syscall
    pop %rsi
    call print_number_str

.skip_ok:
    // error_requests
    lea key_err(%rip), %rdi
    call find_value
    test %rax, %rax
    jz .skip_err
    mov %rax, %rsi
    push %rsi
    mov $1, %rdi
    lea lbl_err(%rip), %rsi
    mov $lbl_err_len, %rdx
    mov $SYS_write, %rax
    syscall
    pop %rsi
    call print_number_str

.skip_err:
    // avg_latency
    lea key_avg(%rip), %rdi
    call find_value
    test %rax, %rax
    jz .skip_avg
    mov %rax, %rsi
    push %rsi
    mov $1, %rdi
    lea lbl_avg(%rip), %rsi
    mov $lbl_avg_len, %rdx
    mov $SYS_write, %rax
    syscall
    pop %rsi
    call print_number_str
    mov $1, %rdi
    lea str_ms(%rip), %rsi
    mov $str_ms_len, %rdx
    mov $SYS_write, %rax
    syscall

.skip_avg:
    // uptime
    lea key_uptime(%rip), %rdi
    call find_value
    test %rax, %rax
    jz .skip_uptime
    mov %rax, %rsi
    push %rsi
    mov $1, %rdi
    lea lbl_uptime(%rip), %rsi
    mov $lbl_uptime_len, %rdx
    mov $SYS_write, %rax
    syscall
    pop %rsi
    call print_number_str
    mov $1, %rdi
    lea str_s(%rip), %rsi
    mov $str_s_len, %rdx
    mov $SYS_write, %rax
    syscall

.skip_uptime:
    call print_sep

    pop %rbp
    ret

// ============================================================
// find_value — buscar "key":VALUE en resp_buf
// Entrada: rdi = puntero a key string
// Salida:  rax = puntero al inicio del número, o 0
// ============================================================
find_value:
    push %rbp
    mov %rsp, %rbp
    push %r12
    push %r13
    push %r14

    mov %rdi, %r12           // key
    lea resp_buf(%rip), %r13 // buffer
    mov $8192, %r14          // max len

    // calcular longitud del key
    mov %r12, %rdi
    call strlen_rdi
    mov %rax, %rcx           // key_len

.scan_loop:
    cmp $0, %r14
    jle .not_found

    // comparar key en posición actual
    mov %r12, %rsi
    mov %r13, %rdi
    push %rcx
    call memcmp_rcx
    pop %rcx
    test %rax, %rax
    jz .found

    inc %r13
    dec %r14
    jmp .scan_loop

.found:
    // avanzar más allá del key
    add %rcx, %r13
    // saltar espacios
1:  movzbl (%r13), %eax
    cmp $' ', %al
    jne 2f
    inc %r13
    jmp 1b
2:  mov %r13, %rax
    jmp .fv_done

.not_found:
    xor %rax, %rax

.fv_done:
    pop %r14
    pop %r13
    pop %r12
    pop %rbp
    ret

// ============================================================
// print_number_str — imprimir número hasta , o }
// Entrada: rsi = puntero al número
// ============================================================
print_number_str:
    push %rbp
    mov %rsp, %rbp
    push %r12
    push %r13

    mov %rsi, %r12
    xor %r13, %r13

    // calcular longitud hasta , } o \r\n
1:  movzbl (%r12, %r13), %eax
    cmp $',', %al
    je 2f
    cmp $'}', %al
    je 2f
    cmp $'\r', %al
    je 2f
    cmp $'\n', %al
    je 2f
    cmp $0, %al
    je 2f
    inc %r13
    jmp 1b
2:
    mov $1, %rdi
    mov %rsi, %rsi
    mov %r13, %rdx
    mov $SYS_write, %rax
    syscall

    // newline
    mov $1, %rdi
    lea str_newline(%rip), %rsi
    mov $1, %rdx
    mov $SYS_write, %rax
    syscall

    pop %r13
    pop %r12
    pop %rbp
    ret

// ============================================================
// print_sep
// ============================================================
print_sep:
    push %rbp
    mov %rsp, %rbp
    mov $1, %rdi
    lea str_sep(%rip), %rsi
    mov $str_sep_len, %rdx
    mov $SYS_write, %rax
    syscall
    pop %rbp
    ret

// ============================================================
// strlen_rdi
// ============================================================
strlen_rdi:
    xor %rax, %rax
1:  movzbl (%rdi, %rax), %ecx
    test %cl, %cl
    jz 2f
    inc %rax
    jmp 1b
2:  ret

// ============================================================
// memcmp_rcx — comparar rcx bytes de rdi y rsi
// ============================================================
memcmp_rcx:
    push %rbp
    mov %rsp, %rbp
    xor %rax, %rax
1:  test %rcx, %rcx
    jz .mc_equal
    movzbl (%rdi), %eax
    movzbl (%rsi), %edx
    cmp %dl, %al
    jne .mc_diff
    inc %rdi
    inc %rsi
    dec %rcx
    jmp 1b
.mc_equal:
    xor %rax, %rax
    jmp .mc_done
.mc_diff:
    mov $1, %rax
.mc_done:
    pop %rbp
    ret
