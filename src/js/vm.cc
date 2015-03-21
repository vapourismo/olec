#include "vm.h"

#include <iostream>
#include <map>
#include <string>
#include <v8.h>
#include <cstdlib>

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
void js_debug_log(const v8::FunctionCallbackInfo<v8::Value>& args) {
	for (int i = 0; i < args.Length(); i++) {
		if (i > 0)
			cout << ' ';

		v8::String::Utf8Value str_value(args[i]->ToString());
		cout << *str_value;
	}

	cout << endl;
}

static
map<String, v8::Handle<v8::Value>> js_modules;

static
Value js_require_absolute(v8::Isolate* isolate, String path) {
	if (js_modules.count(path) > 0) {
		return js_modules[path];
	}

	ScriptFile script(path.c_str());

	v8::Local<v8::Object> global = script.context->Global();
	global->Set(Foreign<String>::generate(isolate, "exports"), v8::Object::New(isolate));

	script.run();

	v8::Local<v8::String> export_key = v8::String::NewFromUtf8(isolate, "exports");
	Value val;

	if (global->Has(export_key)) {
		val = global->Get(export_key);
	} else {
		val = v8::Null(isolate);
	}

	js_modules[path] = val;

	return val;
}

static
Value js_require(String path) {
	v8::Isolate* isolate = v8::Isolate::GetCurrent();

	if (path.empty())
		return v8::Null(isolate);

	if (path[0] == '/') {
		return js_require_absolute(isolate, path);
	}

	char* realpath_cstr = realpath(path.c_str(), nullptr);
	path.assign(realpath_cstr);
	free(realpath_cstr);

	return js_require_absolute(isolate, path);
}

EngineInstance::EngineInstance():
	isolate(v8::Isolate::New()),
	isolate_scope(isolate.get()),
	handle_scope(isolate.get()),
	global_context(isolate.get()),
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
