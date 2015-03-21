#include "vm.h"

#include <iostream>
#include <v8.h>

using namespace std;

namespace olec {
namespace js {

struct V8Initializer {
	V8Initializer() {
		v8::V8::InitializeICU();
		v8::V8::Initialize();
	}

	~V8Initializer() {
		v8::V8::Dispose();
	}
} v8init;

static
Value js_require(String a) {
	ScriptFile script(a.c_str());
	script.run();

	return script.exports();
}

static
void js_debug_log(const v8::FunctionCallbackInfo<v8::Value>& args) {
	for (int i = 0; i < args.Length(); i++) {
		if (i > 0)
			cout << ' ';

		v8::String::Utf8Value str_value(args[i]->ToString());
		cout << *str_value;
	}

	cout << endl;
}

EngineInstance::EngineInstance():
	isolate(v8::Isolate::New()),
	isolate_scope(isolate.get()),
	handle_scope(isolate.get()),
	global_template(isolate.get())
{
	isolate->SetData(0, this);

	// Debug Object
	ObjectTemplate debug_tpl(isolate.get());
	debug_tpl.set("log", v8::FunctionTemplate::New(isolate.get(), js_debug_log));

	// Register fields
	global_template.set("debug", debug_tpl);
	global_template.set("require", js_require);
}

}
}
