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

#include "js/args.h"

#include <iostream>
#include <string>
#include <v8.h>

using namespace std;
using namespace v8;
using namespace olec;

struct JavaScriptVM {
	v8::Isolate* isolate;

	JavaScriptVM();

	~JavaScriptVM();

	inline
	operator v8::Isolate*() {
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
	MyObject(js::Boolean b, js::Integer i, js::Number d) {
		cout << b << endl << i << endl << d << endl;
	}
};

template <typename T, typename... A>
struct ClassTemplate {
	Local<ObjectTemplate> value;

	ClassTemplate(Isolate* isolate):
		value(ObjectTemplate::New(isolate))
	{
		value->SetCallAsFunctionHandler(js::ctor<T, A...>);
		// TODO: Set prototype
	}

	inline
	ObjectTemplate* operator *() {
		return *value;
	}

	inline
	ObjectTemplate* operator ->() {
		return *value;
	}

	template <typename S> inline
	operator Local<S>() {
		return value;
	}

	template <typename S> inline
	operator Handle<S>() {
		return value;
	}
};

int main() {
	Isolate::Scope isolate_scope(jsvm);
	HandleScope handle_scope(jsvm);

	Local<ObjectTemplate> global = ObjectTemplate::New();

	ClassTemplate<MyObject, js::Boolean, js::Integer, js::Number> class_tpl(jsvm);

	global->Set(String::NewFromUtf8(jsvm, "Test"), class_tpl);

	Local<Context> context = Context::New(jsvm, nullptr, global);
	Context::Scope context_scope(context);

	Local<Script> script = Script::Compile(String::NewFromUtf8(jsvm, "new Test(true, 1337, 11273091.23)"));
	Local<Value> result = script->Run();

	String::Utf8Value utf8(result);
	cout << *utf8 << endl;

	return 0;
}
