#include <stdlib.h>
#include <string.h>
#include <pwd.h>
#include <grp.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "ownerutil.h"

uid_t getuid(const char *filepath) {
    struct stat *buf = malloc(sizeof(struct stat));
    stat(filepath, buf);
    uid_t res = buf->st_uid;
    free(buf);
    return res;
}

gid_t getgid(const char *filepath) {
    struct stat *buf = malloc(sizeof(struct stat));
    stat(filepath, buf);
    gid_t res = buf->st_gid;
    free(buf);
    return res;
}

void getlogin_from_uid(uid_t uid, char *str, int size) {
    struct passwd *res = getpwuid(uid);
    strncpy(str, res->pw_name, size);
}

void getgroup_from_gid(gid_t gid, char *str, int size) {
    struct group *res = getgrgid(gid);
    strncpy(str, res->gr_name, size);
}

void get_owner(char filepath[], char str[], int size) {
    uid_t uid = getuid(filepath);
    getlogin_from_uid(uid, str, size);
}

void get_gowner(const char *filepath, char *str, int size) {
    gid_t gid = getgid(filepath);
    getgroup_from_gid(gid, str, size);
}
