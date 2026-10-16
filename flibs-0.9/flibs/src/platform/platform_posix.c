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
