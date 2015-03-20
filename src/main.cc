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

using namespace std;
using namespace v8;
using namespace olec;

struct MyObject {
	js::Boolean a;
	js::Integer b;
	js::Number c;

	MyObject(js::Boolean a, js::Integer b, js::Number c):
		a(a), b(b), c(c)
	{}

	js::Number method(js::String d) {
		cout << a << ", " << b << ", " << c << ", " << d << endl;
		return 13.37;
	}
};

static
void js_print(js::Value a) {
	v8::String::Utf8Value utf8(a);
	cout << *utf8 << endl;
}

int main() {
	js::EngineInstance vm;

	js::ObjectTemplate globals(vm);

	js::ClassTemplate<MyObject, js::Boolean, js::Integer, js::Number> class_tpl(vm);
	class_tpl.method("method", &MyObject::method);
	class_tpl.property("b", &MyObject::b);

	js::ObjectTemplate obj(vm);
	obj.set("value", js::Foreign<js::Integer>::generate(vm, 1337));

	globals.set("data", obj);
	globals.set("Test", class_tpl);
	globals.set("print", js_print);

	js::Context context(vm, globals);
	js::ScriptFile script(vm, "ext/js/entry.js");
	script.run();

	return 0;
}
