/* POSIX: cc -std=c99 -Wall -Wextra -Os memalias.c -lrt
 * MinGW: cc -std=c99 -Wall -Wextra -Os memalias.c
 */
#define _POSIX_C_SOURCE 200112L // ftruncate()
#include <stdio.h>
#include <stdint.h>
#include <assert.h>

int  memory_alias_map(size_t size, size_t naddr, void **addrs);
void memory_alias_unmap(size_t size, size_t naddr, void **addrs);

int
main(void)
{
    size_t size = sizeof(uint32_t);
    void *addr[3] = {0};
    size_t naddr = sizeof(addr) / sizeof(addr[0]);
    int r = memory_alias_map(size, naddr, addr);
    assert(r == 0);
    *(uint32_t *)addr[0] = 0xdeadbeef;
    for (size_t i = 0; i < naddr; i++)
        printf("*%p = 0x%x\n", addr[i], *(uint32_t *)addr[i]);
    *(uint32_t *)addr[0] = 0xcafebabe;
    for (size_t i = 0; i < naddr; i++)
        printf("*%p = 0x%x\n", addr[i], *(uint32_t *)addr[i]);
    memory_alias_unmap(size, naddr, addr);
    return 0;
}

#ifdef __WIN32__
#include <windows.h>

int
memory_alias_map(size_t size, size_t naddr, void **addrs)
{
    HANDLE m = CreateFileMapping(INVALID_HANDLE_VALUE,
                                 NULL,
                                 PAGE_READWRITE,
                                 0, size,
                                 NULL);
    if (m == NULL)
        return -1;
    DWORD access = FILE_MAP_ALL_ACCESS;
    for (size_t i = 0; i < naddr; i++) {
        addrs[i] = MapViewOfFileEx(m, access, 0, 0, size, addrs[i]);
        if (addrs[i] == NULL) {
            memory_alias_unmap(size, i, addrs);
            CloseHandle(m);
            return -1;
        }
    }
    CloseHandle(m);
    return 0;
}

void
memory_alias_unmap(size_t size, size_t naddr, void **addrs)
{
    (void)size;
    for (size_t i = 0; i < naddr; i++)
        UnmapViewOfFile(addrs[i]);
}

#else
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>

int
memory_alias_map(size_t size, size_t naddr, void **addrs)
{
    char path[128];
    snprintf(path, sizeof(path), "/%s(%lu,%p)",
             __FUNCTION__, (long)getpid(), addrs);
    int fd = shm_open(path, O_RDWR | O_CREAT | O_EXCL, 0600);
    if (fd == -1)
        return -1;
    shm_unlink(path);
    ftruncate(fd, size);
    for (size_t i = 0; i < naddr; i++) {
        addrs[i] = mmap(addrs[i], size,
                        PROT_READ | PROT_WRITE, MAP_SHARED,
                        fd, 0);
        if (addrs[i] == MAP_FAILED) {
            memory_alias_unmap(size, i, addrs);
            close(fd);
            return -1;
        }
    }
    close(fd);
    return 0;
}

void
memory_alias_unmap(size_t size, size_t naddr, void **addrs)
{
    for (size_t i = 0; i < naddr; i++)
        munmap(addrs[i], size);
}

#endif
