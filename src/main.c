#include "child.h"
#include "parent.h"

#include <stdlib.h>

int main(int argc, char** argv) {
	if (getenv("OLEC_IPC") != NULL)
		return olec_child_launch(argc, argv);
	else
		return olec_parent_launch(argc, argv);
}
