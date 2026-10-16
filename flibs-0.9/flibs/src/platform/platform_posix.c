/*
 * platform_posix.c --
 *   Small POSIX helpers for m_platform that cannot be bound portably from
 *   Fortran because they depend on struct layouts (struct dirent) or on
 *   platform-specific open(2) flags.
 */
#define _POSIX_C_SOURCE 200809L
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

/* Return the name of the next entry of dir, or NULL at the end. */
const char *flibs_platform_readdir_name(DIR *dir)
{
    struct dirent *entry = readdir(dir);
    return entry ? entry->d_name : NULL;
}

/*
 * Remove name (relative to dirfd) and, if it is a directory, everything
 * below it. Symbolic links are removed, never followed: each directory is
 * opened with O_NOFOLLOW and its entries are removed relative to that
 * descriptor, so swapping a path component for a symlink cannot redirect
 * the deletion outside the tree.
 */
static int remove_tree_at(int dirfd, const char *name)
{
    int fd = openat(dirfd, name, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    if (fd < 0) {
        if (errno == ENOTDIR || errno == ELOOP || errno == EMLINK)
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

int flibs_platform_remove_tree(const char *path)
{
    return remove_tree_at(AT_FDCWD, path);
}

/* Return 1 if path is a directory (following symbolic links), 0 otherwise. */
int flibs_platform_is_directory(const char *path)
{
    struct stat st;
    return stat(path, &st) == 0 && S_ISDIR(st.st_mode);
}

/*
 * Copy the regular file src into dst, as cp does: a new dst gets the
 * permission bits of src (minus the umask), an existing dst keeps its own.
 * Copying a file onto itself is refused without touching it.
 */
int flibs_platform_copy_file(const char *src, const char *dst)
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
    int out = open(dst, O_WRONLY | O_CREAT | O_CLOEXEC, src_st.st_mode & 0777);
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
