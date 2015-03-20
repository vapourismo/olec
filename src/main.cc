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

#include "js/types.h"

#include <iostream>
#include <string>
#include <v8.h>

using namespace std;
using namespace v8;
using namespace olec;

struct JavaScriptVM {
	Isolate* isolate;

	JavaScriptVM();

	~JavaScriptVM();

	inline
	operator Isolate*() {
		return isolate;
	}
};

JavaScriptVM::JavaScriptVM() {
	V8::InitializeICU();
	V8::Initialize();

	isolate = Isolate::New();
}

JavaScriptVM::~JavaScriptVM() {
	isolate->Dispose();
	V8::Dispose();
}

static
JavaScriptVM jsvm;

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

int main() {
	Isolate::Scope isolate_scope(jsvm);
	HandleScope handle_scope(jsvm);

	Local<ObjectTemplate> global = ObjectTemplate::New();

	js::ClassTemplate<MyObject, js::Boolean, js::Integer, js::Number> class_tpl(jsvm);
	class_tpl.method("method", &MyObject::method);
	class_tpl.property("b", &MyObject::b);

	global->Set(String::NewFromUtf8(jsvm, "Test"), class_tpl);

	Local<Context> context = Context::New(jsvm, nullptr, global);
	Context::Scope context_scope(context);

	Local<Script> script =
		Script::Compile(String::NewFromUtf8(jsvm, "var t = new Test(false, 1337, 1091.23); t.b += 63; t.method('Hello');"));
	Local<Value> result = script->Run();

	String::Utf8Value utf8(result);
	cout << *utf8 << endl;

	return 0;
}
