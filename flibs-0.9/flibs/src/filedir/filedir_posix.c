/*
 * filedir_posix.c --
 *   POSIX file-system services for m_filedir. File names are passed to the
 *   C library as data, never to a shell, so shell metacharacters in a name
 *   have no special meaning.
 */
#define _POSIX_C_SOURCE 200809L
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <glob.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#ifndef O_CLOEXEC
#define O_CLOEXEC 0
#endif
#ifndef O_NOCTTY
#define O_NOCTTY 0
#endif

/* Expand a wildcard pattern like the shell does, without running a shell.
   Returns a handle (NULL if none) and the number of sorted matches. */
void *flibs_filedir_glob_open(const char *pattern, size_t *count)
{
    glob_t *g = calloc(1, sizeof *g);
    *count = 0;
    if (g == NULL)
        return NULL;
    if (glob(pattern, 0, NULL, g) == 0)
        *count = g->gl_pathc;
    return g;
}

const char *flibs_filedir_glob_path(void *g, size_t i)
{
    return ((glob_t *)g)->gl_pathv[i];
}

void flibs_filedir_glob_close(void *g)
{
    if (g == NULL)
        return;
    globfree((glob_t *)g);
    free(g);
}

/* Return 1 if path is a directory (following symbolic links), 0 otherwise. */
int flibs_filedir_isdir(const char *path)
{
    struct stat st;
    return stat(path, &st) == 0 && S_ISDIR(st.st_mode);
}

int flibs_filedir_mkdir(const char *path)
{
    return mkdir(path, 0777);
}

/* Create path if needed (as touch does) and set its times to now. */
int flibs_filedir_touch(const char *path)
{
    if (utimensat(AT_FDCWD, path, NULL, 0) == 0)
        return 0;
    if (errno != ENOENT)
        return -1;
    int fd = open(path, O_WRONLY | O_CREAT | O_NOCTTY | O_CLOEXEC, 0666);
    if (fd < 0)
        return -1;
    int status = futimens(fd, NULL);
    if (close(fd) != 0)
        status = -1;
    return status;
}

/*
 * Remove name (relative to dirfd) and, if it is a directory, all its content.
 * Symbolic links are removed, never followed: each directory is opened with
 * O_NOFOLLOW and its entries are removed relative to that descriptor.
 */
static int remove_tree_at(int dirfd, const char *name)
{
    int fd = openat(dirfd, name, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    if (fd < 0) {
        if (errno == ENOTDIR || errno == ELOOP)
            return unlinkat(dirfd, name, 0);
        return -1;
    }
    DIR *dir = fdopendir(fd);
    if (dir == NULL) {
        close(fd);
        return -1;
    }
    int status = 0;
    struct dirent *entry;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
            continue;
        if (remove_tree_at(fd, entry->d_name) != 0)
            status = -1;
    }
    closedir(dir);
    if (status != 0)
        return -1;
    return unlinkat(dirfd, name, AT_REMOVEDIR);
}

int flibs_filedir_remove_tree(const char *path)
{
    return remove_tree_at(AT_FDCWD, path);
}

/*
 * Copy the regular file src into dst, as cp does: a new dst gets the
 * permission bits of src, an existing dst keeps its own. Copying a file
 * onto itself is refused without truncating it.
 */
int flibs_filedir_copy_file(const char *src, const char *dst)
{
    struct stat src_st, dst_st;
    char buffer[65536];
    int status = -1;
    int in = open(src, O_RDONLY | O_CLOEXEC);
    if (in < 0)
        return -1;
    if (fstat(in, &src_st) != 0 || !S_ISREG(src_st.st_mode)) {
        close(in);
        return -1;
    }
    int out = open(dst, O_WRONLY | O_CREAT | O_NOCTTY | O_CLOEXEC, src_st.st_mode & 0777);
    if (out < 0) {
        close(in);
        return -1;
    }
    if (fstat(out, &dst_st) != 0 ||
        (dst_st.st_dev == src_st.st_dev && dst_st.st_ino == src_st.st_ino) ||
        ftruncate(out, 0) != 0)
        goto done;
    for (;;) {
        ssize_t nread = read(in, buffer, sizeof buffer);
        if (nread == 0)
            break;
        if (nread < 0) {
            if (errno == EINTR)
                continue;
            goto done;
        }
        char *p = buffer;
        while (nread > 0) {
            ssize_t nwritten = write(out, p, (size_t)nread);
            if (nwritten < 0) {
                if (errno == EINTR)
                    continue;
                goto done;
            }
            p += nwritten;
            nread -= nwritten;
        }
    }
    status = 0;
done:
    if (close(out) != 0)
        status = -1;
    close(in);
    return status;
}
