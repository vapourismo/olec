#include "vm.h"

#include <iostream>
#include <map>
#include <string>
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

// static
// Value js_require(String a) {
// 	ScriptFile script(a.c_str());
// 	script.run();

// 	return script.exports();
// }

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

template <typename T, typename... A> static
v8::Local<v8::Object> new_object(ObjectTemplate& object_tpl, A... args) {
	object_tpl->SetInternalFieldCount(1);

	// Construct C++ type
	T* instance = new T(args...);
	v8::Local<v8::External> self = v8::External::New(object_tpl.isolate, instance);

	// Instantiate object
	v8::Local<v8::Object> obj = object_tpl->NewInstance();
	obj->SetInternalField(0, self);

	return obj;
}

struct ModuleSystem {
	map<string, v8::Local<v8::Value>> modules;

	ModuleSystem() {

	}

	Value require(String file) {
		return Foreign<String>::generate(EngineInstance::current(), file);
	}
};

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

	// Module Class
	// ObjectTemplate module_tpl(isolate.get());

	// MethodTemplate<ModuleSystem, Value, String> module_require_tpl(isolate.get(), &ModuleSystem::require);
	// module_tpl.set("require", module_require_tpl);

	ClassTemplate<ModuleSystem> module_tpl(isolate.get());
	module_tpl.method("require", &ModuleSystem::require);

	// Register fields
	global_template.set("debug", debug_tpl);
	global_template.set("module", module_tpl.instantiate());
	// global_template.set("module", new_object<ModuleSystem>(module_tpl));
	// global_template.set("require", js_require);
}

}
}
