/*
 * filedir_posix.c --
 *   POSIX file-system services for m_filedir. File names are passed to the
 *   C library as data, never to a shell, so shell metacharacters in a name
 *   have no special meaning.
 */
#define _POSIX_C_SOURCE 200809L
#define _XOPEN_SOURCE 700
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

struct flibs_glob {
    glob_t g;
    size_t *keep;
    size_t n;
};

static int is_dot_entry(const char *path)
{
    const char *base = strrchr(path, '/');
    base = base ? base + 1 : path;
    return strcmp(base, ".") == 0 || strcmp(base, "..") == 0;
}

/* Expand a wildcard pattern like the shell does, without running a shell.
   Returns a handle (NULL if none) and the number of sorted matches;
   "." and ".." entries are skipped. */
void *flibs_filedir_glob_open(const char *pattern, size_t *count)
{
    struct flibs_glob *h = calloc(1, sizeof *h);
    *count = 0;
    if (h == NULL)
        return NULL;
    if (glob(pattern, 0, NULL, &h->g) != 0)
        return h;
    h->keep = malloc((h->g.gl_pathc ? h->g.gl_pathc : 1) * sizeof *h->keep);
    if (h->keep == NULL)
        return h;
    for (size_t i = 0; i < h->g.gl_pathc; i++)
        if (!is_dot_entry(h->g.gl_pathv[i]))
            h->keep[h->n++] = i;
    *count = h->n;
    return h;
}

const char *flibs_filedir_glob_path(void *handle, size_t i)
{
    struct flibs_glob *h = handle;
    return h->g.gl_pathv[h->keep[i]];
}

void flibs_filedir_glob_close(void *handle)
{
    struct flibs_glob *h = handle;
    if (h == NULL)
        return;
    globfree(&h->g);
    free(h->keep);
    free(h);
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

/*
 * Nonzero if the last component of path (ignoring trailing slashes) is
 * "." or "..", or path is empty.
 */
static int is_dot_path(const char *path)
{
    size_t end = strlen(path);
    while (end > 0 && path[end - 1] == '/')
        end--;
    if (end == 0)
        return path[0] == '\0';
    size_t start = end;
    while (start > 0 && path[start - 1] != '/')
        start--;
    size_t n = end - start;
    return (n == 1 && path[start] == '.') ||
           (n == 2 && path[start] == '.' && path[start + 1] == '.');
}

/*
 * Like rm -r -f, refuse ".", ".." (in any position as last component) and
 * the file-system root before anything is removed.
 * A symbolic link is removed itself, whatever it points to.
 */
int flibs_filedir_remove_tree(const char *path)
{
    struct stat st;
    if (is_dot_path(path)) {
        errno = EINVAL;
        return -1;
    }
    if (lstat(path, &st) != 0)
        return -1;
    if (S_ISLNK(st.st_mode))
        return unlink(path);
    char *resolved = realpath(path, NULL);
    if (resolved == NULL)
        return -1;
    int is_root = strcmp(resolved, "/") == 0;
    free(resolved);
    if (is_root) {
        errno = EPERM;
        return -1;
    }
    return remove_tree_at(AT_FDCWD, path);
}

/*
 * Remove an empty directory, or a symbolic link (to a directory) itself,
 * as rm does: the link is never followed.
 */
int flibs_filedir_rmdir(const char *path)
{
    struct stat st;
    if (lstat(path, &st) != 0)
        return -1;
    if (S_ISLNK(st.st_mode))
        return unlink(path);
    return rmdir(path);
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
