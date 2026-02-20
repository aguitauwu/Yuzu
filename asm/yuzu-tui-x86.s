// ============================================================
// Yuzu TUI Monitor — x86 (i686) Linux
// Compilar:
//   as --32 -o yuzu-tui-x86.o yuzu-tui-x86.s
//   ld -m elf_i386 -dynamic-linker /lib/ld-linux.so.2 \
//      -o yuzu-tui-x86 yuzu-tui-x86.o -lc
// ============================================================

.extern getaddrinfo
.extern freeaddrinfo
.extern usleep

.section .data

banner:
.ascii "\033[2J\033[H"
.ascii "\033[38;2;244;114;182m\033[1m"
.ascii "  +-----------------------------------------+\n"
.ascii "  |                                         |\n"
.ascii "  |   * Y U Z U  T U I  x86 (i686)          |\n"
.ascii "  |   API Monitor -- Built in Assembly       |\n"
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
key_ok:     .asciz "ok_requests\":"
key_err:    .asciz "error_requests\":"
key_avg:    .asciz "avg_latency_ms\":"
key_uptime: .asciz "uptime_seconds\":"

.section .bss
.align 4
resp_buf:   .space 8192
hints_buf:  .space 32
addrinfo_p: .space 4
sock_fd:    .space 4

// x86 syscalls
.equ SYS_exit,     1
.equ SYS_read,     3
.equ SYS_write,    4
.equ SYS_close,    6
.equ SYS_socket,   359
.equ SYS_connect,  362
.equ AF_INET,      2
.equ SOCK_STREAM,  1

// addrinfo offsets
.equ AI_FAMILY,    4
.equ AI_SOCKTYPE,  8
.equ AI_ADDR,      20
.equ AI_ADDRLEN,   16

.section .text
.global _start

_start:
    // imprimir banner
    push $banner_len
    push $banner
    push $1
    call write_helper
    add $12, %esp

monitor_loop:
    push $str_home_len
    push $str_home
    push $1
    call write_helper
    add $12, %esp

    call do_http_request
    test %eax, %eax
    js show_offline

    call display_metrics
    jmp sleep_and_loop

show_offline:
    push $lbl_status_len
    push $lbl_status
    push $1
    call write_helper
    add $12, %esp
    push $str_offline_len
    push $str_offline
    push $1
    call write_helper
    add $12, %esp

sleep_and_loop:
    push $2000000
    call usleep
    add $4, %esp
    jmp monitor_loop

// write_helper(fd, buf, len)
write_helper:
    push %ebp
    mov %esp, %ebp
    mov 8(%ebp),  %ebx   // fd
    mov 12(%ebp), %ecx   // buf
    mov 16(%ebp), %edx   // len
    mov $SYS_write, %eax
    int $0x80
    pop %ebp
    ret

// ============================================================
do_http_request:
    push %ebp
    mov %esp, %ebp
    push %esi
    push %edi

    // limpiar hints (32 bytes)
    mov $hints_buf, %edi
    xor %eax, %eax
    mov $8, %ecx
    rep stosl

    movl $AF_INET, hints_buf + AI_FAMILY
    movl $SOCK_STREAM, hints_buf + AI_SOCKTYPE

    // getaddrinfo
    push $addrinfo_p
    push $hints_buf
    push $api_port
    push $api_host
    call getaddrinfo
    add $16, %esp
    test %eax, %eax
    jnz .req32_fail

    mov addrinfo_p, %esi      // *addrinfo
    mov AI_ADDR(%esi), %edi   // sockaddr
    mov AI_ADDRLEN(%esi), %ecx

    // socket
    push $0
    push $SOCK_STREAM
    push $AF_INET
    mov $SYS_socket, %eax
    // en x86 Linux socket usa socketcall (#102)
    // usamos libc indirectamente via linker — pero aquí usamos syscall directa
    // para simplificar: llamamos via libc ya que linkamos -lc
    call socket_via_libc
    test %eax, %eax
    js .req32_fail
    mov %eax, sock_fd

    // connect
    push %ecx
    push %edi
    push %eax
    call connect_via_libc
    add $12, %esp
    test %eax, %eax
    js .req32_fail

    push $addrinfo_p
    call freeaddrinfo
    add $4, %esp

    // write request
    push $http_req_len
    push $http_req
    mov sock_fd, %eax
    push %eax
    call write_helper
    add $12, %esp

    // read response
    push $8191
    push $resp_buf
    mov sock_fd, %eax
    push %eax
    mov $SYS_read, %eax
    int $0x80
    add $12, %esp
    mov %eax, %esi

    // close
    mov sock_fd, %ebx
    mov $SYS_close, %eax
    int $0x80

    mov %esi, %eax
    jmp .req32_done

.req32_fail:
    mov $-1, %eax

.req32_done:
    pop %edi
    pop %esi
    pop %ebp
    ret

// Para i686, socket y connect van via libc (linkamos -lc)
.extern socket
.extern connect
socket_via_libc:
    jmp socket
connect_via_libc:
    jmp connect

// ============================================================
display_metrics:
    push %ebp
    mov %esp, %ebp

    // STATUS ONLINE
    push $str_ok_len
    push $str_ok
    push $1
    call write_helper
    add $12, %esp

    push $lbl_status_len
    push $lbl_status
    push $1
    call write_helper
    add $12, %esp

    call print_sep32

    // total
    push $key_total
    call find_value32
    add $4, %esp
    test %eax, %eax
    jz .dm32_t
    push %eax
    push $lbl_total_len
    push $lbl_total
    push $1
    call write_helper
    add $12, %esp
    pop %eax
    push %eax
    call print_delim32
    add $4, %esp
    push $1
    push $str_newline
    push $1
    call write_helper
    add $12, %esp
.dm32_t:

    // ok
    push $key_ok
    call find_value32
    add $4, %esp
    test %eax, %eax
    jz .dm32_ok
    push %eax
    push $lbl_ok_len
    push $lbl_ok
    push $1
    call write_helper
    add $12, %esp
    pop %eax
    push %eax
    call print_delim32
    add $4, %esp
    push $1
    push $str_newline
    push $1
    call write_helper
    add $12, %esp
.dm32_ok:

    // err
    push $key_err
    call find_value32
    add $4, %esp
    test %eax, %eax
    jz .dm32_err
    push %eax
    push $lbl_err_len
    push $lbl_err
    push $1
    call write_helper
    add $12, %esp
    pop %eax
    push %eax
    call print_delim32
    add $4, %esp
    push $1
    push $str_newline
    push $1
    call write_helper
    add $12, %esp
.dm32_err:

    // avg lat
    push $key_avg
    call find_value32
    add $4, %esp
    test %eax, %eax
    jz .dm32_avg
    push %eax
    push $lbl_avg_len
    push $lbl_avg
    push $1
    call write_helper
    add $12, %esp
    pop %eax
    push %eax
    call print_delim32
    add $4, %esp
    push $str_ms_len
    push $str_ms
    push $1
    call write_helper
    add $12, %esp
.dm32_avg:

    // uptime
    push $key_uptime
    call find_value32
    add $4, %esp
    test %eax, %eax
    jz .dm32_up
    push %eax
    push $lbl_uptime_len
    push $lbl_uptime
    push $1
    call write_helper
    add $12, %esp
    pop %eax
    push %eax
    call print_delim32
    add $4, %esp
    push $str_s_len
    push $str_s
    push $1
    call write_helper
    add $12, %esp
.dm32_up:

    call print_sep32

    pop %ebp
    ret

// find_value32(key) -> eax = ptr o 0
find_value32:
    push %ebp
    mov %esp, %ebp
    push %esi
    push %edi
    push %ebx

    mov 8(%ebp), %esi     // key
    mov $resp_buf, %edi
    mov $8192, %ecx

.fv32_loop:
    jecxz .fv32_miss
    push %ecx
    push %esi
    push %edi
    call prefix_match32
    add $8, %esp
    pop %ecx
    test %eax, %eax
    jz .fv32_hit
    inc %edi
    dec %ecx
    jmp .fv32_loop

.fv32_hit:
    // avanzar past key
    push %esi
    call strlen32
    add $4, %esp
    add %eax, %edi
    // skip spaces
1:  movzbl (%edi), %eax
    cmp $' ', %al
    jne 2f
    inc %edi
    jmp 1b
2:  mov %edi, %eax
    jmp .fv32_done

.fv32_miss:
    xor %eax, %eax

.fv32_done:
    pop %ebx
    pop %edi
    pop %esi
    pop %ebp
    ret

// prefix_match32(haystack, needle) -> 0=match
prefix_match32:
    push %ebp
    mov %esp, %ebp
    push %esi
    push %edi
    mov 8(%ebp), %edi
    mov 12(%ebp), %esi
1:  movzbl (%esi), %eax
    test %al, %al
    jz .pm32_ok
    movzbl (%edi), %ecx
    cmp %cl, %al
    jne .pm32_fail
    inc %edi
    inc %esi
    jmp 1b
.pm32_ok:
    xor %eax, %eax
    jmp .pm32_done
.pm32_fail:
    mov $1, %eax
.pm32_done:
    pop %edi
    pop %esi
    pop %ebp
    ret

// strlen32(str) -> eax
strlen32:
    push %ebp
    mov %esp, %ebp
    mov 8(%ebp), %ecx
    xor %eax, %eax
1:  movzbl (%ecx, %eax), %edx
    test %dl, %dl
    jz 2f
    inc %eax
    jmp 1b
2:  pop %ebp
    ret

// print_delim32(ptr)
print_delim32:
    push %ebp
    mov %esp, %ebp
    push %esi
    mov 8(%ebp), %esi
    xor %ecx, %ecx
1:  movzbl (%esi, %ecx), %eax
    cmp $',', %al
    je 2f
    cmp $'}', %al
    je 2f
    cmp $'\r', %al
    je 2f
    cmp $'\n', %al
    je 2f
    test %al, %al
    jz 2f
    inc %ecx
    jmp 1b
2:  mov %ecx, %edx
    mov %esi, %ecx
    mov $1, %ebx
    mov $SYS_write, %eax
    int $0x80
    pop %esi
    pop %ebp
    ret

print_sep32:
    push $str_sep_len
    push $str_sep
    push $1
    call write_helper
    add $12, %esp
    ret
