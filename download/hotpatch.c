/* gcc -std=c99 -Wall -Wextra -Os -pthread hotpatch.c */
#define _BSD_SOURCE // usleep()
#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <unistd.h>
#include <pthread.h>
#include <sys/mman.h>

__attribute__ ((ms_hook_prologue))
__attribute__ ((aligned(8)))
__attribute__ ((noinline))
__attribute__ ((noclone))
void
hello(void)
{
    __asm("");
    puts("hello");
}

void
goodbye(void)
{
    static int x;
    printf("goodbye %d\n", x++);
}

void
hotpatch(void *target, void *replacement)
{
    assert(((uintptr_t)target & 0x07) == 0);
    void *page = (void *)((uintptr_t)target & ~0xfff);
    mprotect(page, 4096, PROT_WRITE | PROT_EXEC);
    uint32_t rel = (char *)replacement - (char *)target - 5;
    union {
        uint8_t bytes[8];
        uint64_t value;
    } instruction = {{0xe9, rel >> 0, rel >> 8, rel >> 16, rel >> 24}};
    *(uint64_t *)target = instruction.value;
    mprotect(page, 4096, PROT_EXEC);
}

void *
worker(void *arg)
{
    (void)arg;
    for (;;) {
        hello();
        usleep(100000);
    }
    return NULL;
}

int
main(void)
{
    pthread_t thread;
    pthread_create(&thread, NULL, worker, NULL);
    getchar();
    hotpatch(hello, goodbye);
    pthread_join(thread, NULL);
    return 0;
}
