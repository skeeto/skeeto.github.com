/* http://redd.it/2zna5q
 * Fibonacci example:
 *     (1) (2) +
 *     0:0
 *     1:1
 *     20
 */
#define _BSD_SOURCE  // MAP_ANONYMOUS
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>

struct asmbuf {
    size_t nconstants;
    double constants[32];
    size_t size, fill;
    uint8_t code[];
};

#ifdef __WIN32__
#include <windows.h>

struct asmbuf *
asmbuf_create(void)
{
    SYSTEM_INFO system_info;
    GetSystemInfo(&system_info);
    long page_size = system_info.dwPageSize;
    struct asmbuf *buf =
        VirtualAlloc(NULL, page_size, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
    buf->size = page_size;
    return buf;
}

void
asmbuf_free(struct asmbuf *buf)
{
    VirtualFree(buf, buf->size, MEM_RELEASE);
}

void
asmbuf_finalize(struct asmbuf *buf)
{
    DWORD old_protect;
    VirtualProtect(buf, buf->size, PAGE_EXECUTE_READ, &old_protect);
}
#else /* POSIX */
#include <sys/mman.h>

struct asmbuf *
asmbuf_create(void)
{
    long page_size = sysconf(_SC_PAGESIZE);
    int prot = PROT_READ | PROT_WRITE;
    int flags = MAP_ANONYMOUS | MAP_PRIVATE;
    struct asmbuf *buf = mmap(NULL, page_size, prot, flags, -1, 0);
    buf->size = page_size;
    return buf;
}

void
asmbuf_free(struct asmbuf *buf)
{
    munmap(buf, buf->size);
}

void
asmbuf_finalize(struct asmbuf *buf)
{
    mprotect(buf, buf->size, PROT_READ | PROT_EXEC);
}
#endif

void
asmbuf_ins(struct asmbuf *buf, int size, uint64_t ins)
{
    for (int i = size - 1; i >= 0; i--)
        buf->code[buf->fill++] = (ins >> (i * 8)) & 0xff;
}

void
asmbuf_immediate(struct asmbuf *buf, int size, const void *value)
{
    memcpy(buf->code + buf->fill, value, size);
    buf->fill += size;
}

static void
operator2(struct asmbuf *buf, int size, uint64_t op)
{
    asmbuf_ins(buf, 6, 0x660f1244cff0); // movlpd  xmm0, [rdi + rcx*8 - 16]
    asmbuf_ins(buf, 6, 0x660f124ccff8); // movlpd  xmm1, [rdi + rcx*8 - 8]
    asmbuf_ins(buf, size, op);
    asmbuf_ins(buf, 3, 0x48ffc9);       // dec     rcx
    asmbuf_ins(buf, 6, 0x660f1344cff8); // movlpd  [rdi + rcx*8 - 8], xmm0
}

int
main(void)
{
    /* rdi  : pointer to the base of the stack
     * rsi  : index of the top of the stack when called
     * rcx  : index of the current top of the stack
     * xmm* : intermediate results
     */
    struct asmbuf *buf = asmbuf_create();
    long max_backreference = 0;
    long max_stack = 0;
    long current_stack = 0;
    asmbuf_ins(buf, 3, 0x4889f1);  // mov   rcx, rsi
    int c;
    while ((c = fgetc(stdin)) != '\n' && c != EOF) {
        if (c == ' ')
            continue;
        switch (c) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9': {
            ungetc(c, stdin);
            double *data = &buf->constants[buf->nconstants++];
            scanf("%lf", data);
            asmbuf_ins(buf, 2, 0x48a1);     // mov   rax, data
            asmbuf_immediate(buf, 8, &data);
            asmbuf_ins(buf, 4, 0x488904cf); // mov   [rdi + rcx*8], rax
            asmbuf_ins(buf, 3, 0x48ffc1);   // inc   rcx
            if (++current_stack > max_stack)
                max_stack = current_stack;
        } break;
        case '(': {
            int n;
            scanf("%d)", &n);
            if (n > max_backreference)
                max_backreference = n;
            int8_t offset = -8 * n;
            asmbuf_ins(buf, 4, 0x488b44f7); // mov   rax, [rdi+rsi*8-offset]
            asmbuf_immediate(buf, 1, &offset);
            asmbuf_ins(buf, 4, 0x488904cf); // mov   [rdi + rcx*8], rax
            asmbuf_ins(buf, 3, 0x48ffc1);   // inc   rcx
            if (++current_stack > max_stack)
                max_stack = current_stack;
        } break;
        case '+':
            operator2(buf, 4, 0xf20f58c1);     // addsd  xmm0,xmm1
            current_stack--;
            break;
        case '-':
            operator2(buf, 4, 0xf20f5cc1);     // subsd  xmm0,xmm1
            current_stack--;
            break;
        case '*':
            operator2(buf, 4, 0xf20f59c1);     // mulsd  xmm0,xmm1
            current_stack--;
            break;
        case '/':
            operator2(buf, 4, 0xf20f5ec1);     // divsd  xmm0,xmm1
            current_stack--;
            break;
        case 'Q':
            asmbuf_ins(buf, 6, 0x660f124ccff8); // movlpd xmm1, [rdi+rcx*8-8]
            asmbuf_ins(buf, 4, 0xf20f51c9);     // sqrtsd xmm1, xmm1
            asmbuf_ins(buf, 6, 0x660f134ccff8); // movlpd [rdi+rcx*8-8], xmm1
            break;
        }
    }
    asmbuf_ins(buf, 3, 0x4889c8); // mov   rax, rcx
    asmbuf_ins(buf, 1, 0xc3);     // retq
    asmbuf_finalize(buf);

    __attribute__ ((sysv_abi))
        long (*recurrence)(double *, long) = (void *)buf->code;
    double stack[max_backreference + max_stack];

    /* Accept up to `max_backreference` initial values. */
    long nterms;
    for (;;) {
        char line[256];
        fgets(line, sizeof(line), stdin);
        long n;
        double value;
        if (sscanf(line, "%ld:%lf", &n, &value) != 2) {
            nterms = strtol(line, NULL, 10);
            break;
        } else {
            stack[n] = value;
        }
    }

    /* Compute terms. */
    for (long n = 0; n < max_backreference; n++)
        printf("%ld: %.15f\n", n, stack[n]);
    for (long n = max_backreference; n <= nterms;) {
        long nelements = recurrence(stack, max_backreference);
        long nvalues = nelements - max_backreference;
        for (long i = 0; i < nvalues; i++)
            printf("%ld: %.15f\n", n + i, stack[max_backreference + i]);
        memmove(stack, stack + nvalues, max_backreference * sizeof(stack[0]));
        n += nvalues;
    }
    return 0;
}
