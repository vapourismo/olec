#include "vm.h"

#include <iostream>
#include <map>
#include <string>
#include <v8.h>
#include <cstdlib>
#include <unistd.h>
#include <libgen.h>

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
		if (i > 0) cout << ' ';

		v8::String::Utf8Value str_value(args[i]->ToString());
		cout << *str_value;
	}

	cout << endl;
}

static
v8::Local<v8::StackFrame> js_get_caller(v8::Isolate* isolate) {
	v8::Local<v8::StackTrace> trace =
		v8::StackTrace::CurrentStackTrace(isolate, 1);

	if (trace->GetFrameCount() == 0)
		return v8::Local<v8::StackFrame>();
	else
		return trace->GetFrame(0);
}

static inline
bool is_readable(const string& path) {
	return access(path.c_str(), R_OK) == 0;
}

static
Value js_require_file(v8::Isolate* isolate, String path) {
	// Generate absolute path
	char* realpath_cstr = realpath(path.c_str(), nullptr);

	// Does the target path not exist?
	if (!realpath_cstr) {
		isolate->ThrowException(
			Foreign<String>::generate(isolate, "ScriptError: Could not find '" + path + "'")
		);

		return Value();
	}

	path.assign(realpath_cstr);
	free(realpath_cstr);

	// Fetch loaded modules handle
	map<String, v8::Handle<v8::Value>>& modules = EngineInstance::current()->modules;

	// Is module already present?
	if (modules.count(path) > 0)
		return modules[path];

	// Compile script
	ScriptFile script(path.c_str());

	// Setup gloal environment
	v8::Local<v8::Object> global = script.context->Global();
	global->Set(Foreign<String>::generate(isolate, "exports"), v8::Object::New(isolate));

	// Launch script
	script.run();

	// Fetch exports
	v8::Local<v8::String> export_key = v8::String::NewFromUtf8(isolate, "exports");
	Value val;

	if (global->Has(export_key)) {
		val = global->Get(export_key);
	} else {
		val = v8::Null(isolate);
	}

	// Submit exports
	modules[path] = val;

	return val;
}

static
Value js_require(String path) {
	v8::Isolate* isolate = v8::Isolate::GetCurrent();

	if (path.empty())
		return v8::Null(isolate);

	// Absolute path
	if (path[0] == '/')
		return js_require_file(isolate, path);

	// Get caller
	v8::Local<v8::StackFrame> caller = js_get_caller(isolate);
	v8::String::Utf8Value script_name(caller->GetScriptName());

	// Get the script's directory
	char* script_dir_cstr = dirname(*script_name);
	String script_dir = script_dir_cstr ? script_dir_cstr : ".";

	String local_path = script_dir + "/" + path;

	if (path.substr(0, 2) == "./" || is_readable(local_path)) {
		return js_require_file(isolate, local_path);
	} else if (is_readable(path)) {
		return js_require_file(isolate, path);
	} else {
		// In case the file could not be found
		isolate->ThrowException(
			Foreign<String>::generate(isolate, "ScriptError: Could not find '" + path + "' or '" + local_path + "'")
		);

		return Value();
	}
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
