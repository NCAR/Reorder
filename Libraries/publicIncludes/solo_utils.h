
/* c------------------------------------------------------------------------ */
# ifndef SOLO_LIST_MGMT
# define SOLO_LIST_MGMT
# define SLM_CODE

struct solo_list_mgmt {
    int num_entries;
    int sizeof_entries;
    int max_entries;
    char **list;
};
/* c------------------------------------------------------------------------ */

struct solo_str_mgmt {
    struct solo_str_mgmt *last;
    struct solo_str_mgmt *next;
    char *at;
};
# endif
/* c------------------------------------------------------------------------ */






