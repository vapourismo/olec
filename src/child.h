#ifndef OLEC_CHILD_H
#define OLEC_CHILD_H

#define OLEC_CHILD_EXIT_OK     0
#define OLEC_CHILD_EXIT_RELOAD 1
#define OLEC_CHILD_EXIT_ERROR  2


/**
 * Launch the main program as a child.
 */
int olec_child_launch(int argc, char** argv);

#endif
