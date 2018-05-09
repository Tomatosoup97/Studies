#include <assert.h>
#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <stdbool.h>

#define DEBUG 0

#define IS_LEAF(node) ((node)->left == NULL && (node)->right == NULL)
#define IS_RIGHT_SON(node) \
    ((node)->parent->right != NULL && \
     (node)->parent->right->key == (node)->key \
)
#define IS_LEFT_SON(node) (!IS_RIGHT_SON(node))
#define IS_ROOT(node) ((node)->parent == NULL)
#define IS_PRIORITY_VALID(node)\
    ((node)->parent == NULL || \
     (node)->parent->priority > (node)->priority \
)
#define HAS_ONLY_RIGHT_SON(node) ((node)->right != NULL && (node)->left == NULL)
#define HAS_ONLY_LEFT_SON(node) ((node)->left != NULL && (node)->right == NULL)

#define ASSERT_NO_SUBTREE_CYCLE(parent, subtree) ({\
    assert(subtree == NULL || \
           (subtree->left != parent && subtree->right != parent));\
})

#define ASSERT_NO_CYCLE(node) ({\
    if (node != NULL) {\
        assert(\
            (node->right == NULL || node->right != node->parent) && \
            (node->left == NULL || node->left != node->parent));\
        ASSERT_NO_SUBTREE_CYCLE(node, node->left);\
        ASSERT_NO_SUBTREE_CYCLE(node, node->right);\
    }\
})

/* Treap data structure implementation */

typedef struct node {
    long long key;
    struct node *right;
    struct node *left;
    struct node *parent;
    int priority;
} node_t;

node_t *create_new_node(node_t *parent, long long x) {
    node_t *node = (node_t*) malloc(sizeof(node_t));
    node->key = x;
    node->priority = rand();
    node->left = NULL;
    node->right = NULL;
    node->parent = parent;

    return node;
}

void show_treap_pointer(node_t *node, const char *name) {
    printf("%s=(%p, %lld) ", name, node, node == NULL ? (-1) : node->key);
}

void show_treap_node(node_t *node) {
    show_treap_pointer(node, "self");
    if (node != NULL) {
        show_treap_pointer(node->left, "left");
        show_treap_pointer(node->right, "right");
        show_treap_pointer(node->parent, "parent");
        printf("priority=%d", node->priority);
    }
    printf("\n");
}

/* void _show_treap_tree_aux(node_t *node, std::string indent, bool is_right) { */
/*     ASSERT_NO_CYCLE(node); */
/*     if (node == NULL) return; */
/*     /1* std::cout << indent << "+- "; *1/ */
/*     show_treap_node(node); */
/*     indent += is_right ? "   " : "|  "; */

/*     _show_treap_tree_aux(node->left, indent, false); */
/*     _show_treap_tree_aux(node->right, indent, true); */
/* } */

/* void show_treap_tree(node_t *root) { */
/*     _show_treap_tree_aux(root, "", true); */
/* } */

node_t *bst_insert(node_t *node, long long x) {
    if (node == NULL) return create_new_node(NULL, x);
    ASSERT_NO_CYCLE(node);

    if (node->key < x) {
        if (node->right == NULL) {
            node->right = create_new_node(node, x);
            return node->right;
        }
        else return bst_insert(node->right, x);
    }
    else if (node->key > x) {
        if (node->left == NULL) {
            node->left = create_new_node(node, x);
            return node->left;
        }
        else return bst_insert(node->left, x);
    } else {
        return node;
    }
}

node_t *bst_search(node_t *node, long long x) {
    if (node == NULL || node->key == x) return node;
    else if (x > node->key)
        return bst_search(node->right, x);
    else
        return bst_search(node->left, x);
}

node_t *bst_search_upper(node_t *root, node_t *upper, long long x) {
    if (root == NULL) return upper;

    if (x == root->key)
        return root;
    else if (x < root->key)
        return bst_search_upper(root->left, root, x);
    else
        return bst_search_upper(root->right, upper, x);
}

node_t *bst_search_lower(node_t *root, node_t *lower, long long x) {
    if (root == NULL) return lower;

    if (x == root->key)
        return root;
    else if (x < root->key)
        return bst_search_lower(root->left, lower, x);
    else
        return bst_search_lower(root->right, root, x);
}

void bst_delete_leaf(node_t **root, node_t *node) {
    assert(IS_LEAF(node));

    if (!IS_ROOT(node)) {
        if (IS_RIGHT_SON(node)) node->parent->right = NULL;
        else node->parent->left = NULL;
    } else {
        *root = NULL;
    }
    free(node);
}

void treap_rotate(node_t **root, node_t *node) {
    assert(!IS_ROOT(node));
    node_t *rotated_parent = node->parent;
    node_t *grandparent = rotated_parent->parent;

    if (IS_RIGHT_SON(node)) {
        // rotate left
        node_t *left_subtree = node->left;
        node->left = rotated_parent;
        rotated_parent->right = left_subtree;
        if (left_subtree != NULL)
            left_subtree->parent = rotated_parent;
    } else {
        // rotate right
        node_t *right_subtree = node->right;
        node->right = rotated_parent;
        rotated_parent->left = right_subtree;
        if (right_subtree != NULL)
            right_subtree->parent = rotated_parent;
    }

    node->parent = grandparent;
    rotated_parent->parent = node;

    if (grandparent != NULL) {
        if (grandparent->right != NULL && \
                grandparent->right->key == rotated_parent->key)
            grandparent->right = node;
        else
            grandparent->left = node;
    } else {
        assert(IS_ROOT(node));
        *root = node;
    }
    // assert no cycles created after rotation
    ASSERT_NO_CYCLE(node);
    ASSERT_NO_CYCLE(rotated_parent);
    ASSERT_NO_CYCLE(grandparent);
}

node_t *treap_insert(node_t **root, long long x) {
    node_t *new_node = bst_insert(*root, x);

    while (!IS_ROOT(new_node) && !IS_PRIORITY_VALID(new_node))
        treap_rotate(root, new_node);

    return new_node;
}

bool treap_delete(node_t **root, long long x) {
    node_t *node = bst_search(*root, x);
    if (node == NULL) return false;

    while (!IS_LEAF(node))
        if (HAS_ONLY_RIGHT_SON(node))
            treap_rotate(root, node->right);
        else if (HAS_ONLY_LEFT_SON(node))
            treap_rotate(root, node->left);
        else if (node->right->priority > node->left->priority)
            treap_rotate(root, node->right);
        else
            treap_rotate(root, node->left);

    bst_delete_leaf(root, node);
    return true;
}

node_t *treap_upper(node_t *root, long long x) {
    return bst_search_upper(root, NULL, x);
}

node_t *treap_lower(node_t *root, long long x) {
    return bst_search_lower(root, NULL, x);
}

void printf_search_result(node_t *node) {
    if (node == NULL) printf("BRAK\n");
    else printf("%lld\n", node->key);
}

void process_queries(int q) {
    node_t *node;
    node_t *root = NULL;
    long long x;
    bool was_deleted;
    char op;

    for (int i=0; i<q; i++) {
        scanf("%c %lld\n", &op, &x);
        if (DEBUG) printf("\n\n%c %lld\n", op, x);

        switch (op) {
            case 'I':
                node = treap_insert(&root, x);
                if (root == NULL) root = node;
                /* if (DEBUG) show_treap_tree(root); */
                break;
            case 'D':
                was_deleted = treap_delete(&root, x);
                printf(was_deleted ? "OK\n" : "BRAK\n");
                /* if (DEBUG) show_treap_tree(root); */
                break;
            case 'U':
                node = treap_upper(root, x);
                printf_search_result(node);
                break;
            case 'L':
                node = treap_lower(root, x);
                printf_search_result(node);
                break;
        }
        assert(root == NULL || IS_ROOT(root));
    }
}

int main() {
    int q;
    srand(time(NULL));
    scanf("%d\n", &q);
    process_queries(q);
    return 0;
}

