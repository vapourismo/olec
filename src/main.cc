#include "terminal.h"

#include <iostream>
#include <unistd.h>
#include <fcntl.h>

using namespace std;
using namespace Olec;

int main(int argc, char** argv) {
	char* ipc_path = getenv("OLEC_IPC");

	if (ipc_path) {
		int fd = open(ipc_path, O_RDONLY | O_NONBLOCK);

		// TODO: Do something

		close(fd);
	} else {
		gtk_init(&argc, &argv);

		Terminal term;

		term.show();
		term.spawn({
			argv[0]
		});

		gtk_main();
	}

	return 0;
}
