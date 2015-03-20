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

static
js::JavaScriptVM jsvm;

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
void f(int a) {
	cout << a << endl;
}

int main() {
	Isolate::Scope isolate_scope(jsvm);
	HandleScope handle_scope(jsvm);

	js::ObjectTemplate globals(jsvm);

	js::ClassTemplate<MyObject, js::Boolean, js::Integer, js::Number> class_tpl(jsvm);
	class_tpl.method("method", &MyObject::method);
	class_tpl.property("b", &MyObject::b);
	globals.set("Test", class_tpl);

	js::FunctionTemplate<void, int> func_tpl(jsvm, f);
	globals.set("f", func_tpl);

	Local<Context> context = Context::New(jsvm, nullptr, globals);
	Context::Scope context_scope(context);

	js::ScriptFile(jsvm, "ext/js/entry.js")->Run();

	return 0;
}
