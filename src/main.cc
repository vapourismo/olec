#include "terminal.h"
#include "app.h"

#include <iostream>
#include <unistd.h>
#include <fcntl.h>

using namespace std;
using namespace olec;

int main(int argc, char** argv) {
	char* ipc_path = getenv("OLEC_IPC");

	if (ipc_path) {
		Application app(ipc_path);
		return app.main();
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
