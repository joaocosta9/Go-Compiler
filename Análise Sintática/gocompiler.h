#include <string.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct node{
    char *id;
    char *type;
    struct node *brother;
    struct node *son;
}node;

void printTree(node* head, int no);

