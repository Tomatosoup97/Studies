
char *strrchr(const char *cp, int ch) {
    char *p = (char *)cp;
    for (char *save = 0;; ++p) {
        if (*p == ch)
            save = p;
        if (*p == '\0')
            return save;
    }
}

