// #include "terminal.h"
// #include "app.h"

// #include <iostream>
// #include <unistd.h>
// #include <fcntl.h>

// using namespace std;
// using namespace olec;

// int main(int argc, char** argv) {
// 	char* ipc_path = getenv("OLEC_IPC");

// 	if (ipc_path) {
// 		Application app(ipc_path);

// 		app.key_map.bind(GDK_CONTROL_MASK, GDK_KEY_q, [&app](KeyModifier mod, KeySymbol key) {
// 			app.exit();
// 			return true;
// 		});

// 		return app.main();
// 	} else {
// 		gtk_init(&argc, &argv);

// 		Terminal term;

// 		term.show();
// 		term.spawn({argv[0]});

// 		gtk_main();
// 	}

// 	return 0;
// }

#include "js/js.h"

#include <iostream>
#include <string>
#include <v8.h>
#include <ncurses.h>

using namespace std;
using namespace olec;
using namespace olec::js;

static
void js_print(Value a) {
	v8::String::Utf8Value utf8(a);
	cout << *utf8 << endl;
}

static
Value js_require(String a) {
	TryCatch tc;
	ScriptFile script(a.c_str());
	script.run();

	if (tc.HasCaught())
		tc.ReThrow();

	return script.exports();
}

int main() {
	EngineInstance vm;

	// Debug printing
	vm.global_template.set("print", js_print);
	vm.global_template.set("require", js_require);

	// Enter script context
	try {
		TryCatch catcher;

		ScriptFile script("ext/js/entry.js");
		catcher.check();

		script.run();
		catcher.check();
	} catch (Exception e) {
		cout << e.what() << endl;
	}

	return 0;
}
