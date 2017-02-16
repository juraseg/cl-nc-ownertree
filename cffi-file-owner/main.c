#include <stdio.h>
#include <stdlib.h>

#include "util.h"


int main(void) {
    char *path = "/Users/juraseg/asd.html";
    printf("Getting info about %s\n", path);
    /* uid_t uid = getuid(path); */
    char *login = malloc(100);
    getlogin(path, login, 99);
    /* getlogin_from_uid(uid, login, 99); */
    /* gid_t gid = getgid(path); */
    char *group = malloc(100);
    getgroup(path, group, 99);
    /* getgroup_from_gid(gid, group, 99); */
    printf("Login: %s. Group: %s\n", login, group);
    free(login);
}
