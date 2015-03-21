#ifndef OLEC_JS_VM_H_
#define OLEC_JS_VM_H_

#include "tpls.h"
#include "exc.h"

#include <v8.h>
#include <memory>
#include <fstream>
#include <string>

namespace olec {
namespace js {

/**
 * V8 Engine Instance
 */
struct EngineInstance {
	struct IsolateDeleter {
		void operator ()(v8::Isolate* isolate) {
			isolate->Dispose();
		}
	};

	std::unique_ptr<v8::Isolate, IsolateDeleter> isolate;
	v8::Isolate::Scope isolate_scope;
	v8::HandleScope handle_scope;

	ObjectTemplate global_template;

	inline
	EngineInstance():
		isolate(v8::Isolate::New()),
		isolate_scope(isolate.get()),
		handle_scope(isolate.get()),
		global_template(isolate.get())
	{
		isolate->SetData(0, this);
	}

	inline
	operator v8::Isolate*() const {
		return isolate.get();
	}

	inline
	void require(const char* file_path) {

	}
};

/**
 * Context
 */
struct Context {
	v8::Local<v8::Context> context;
	v8::Context::Scope context_scope;

	inline
	Context(EngineInstance& vm):
		context(v8::Context::New(vm, nullptr, vm.global_template)),
		context_scope(context)
	{}

	inline
	Context():
		Context(* (EngineInstance*) v8::Isolate::GetCurrent()->GetData(0))
	{}
};

/**
 * Load files as an executable script
 */
struct ScriptFile: Context {
	static inline
	v8::Local<v8::String> read(v8::Isolate* isolate, const char* file_path) {
		std::string contents;
		std::ifstream source(file_path);

		source.seekg(0, std::ios::end);
		contents.reserve(source.tellg());

		source.seekg(0, std::ios::beg);
		contents.assign(std::istreambuf_iterator<char>(source), std::istreambuf_iterator<char>());

		return v8::String::NewFromUtf8(isolate, contents.c_str());
	}

	static inline
	v8::Local<v8::Script> compile(v8::Isolate* isolate, const char* file_path)
		throw (Exception)
	{
		v8::Local<v8::String> contents = read(isolate, file_path);

		v8::TryCatch me;
		v8::Local<v8::Script> script =
			v8::Script::Compile(contents, v8::String::NewFromUtf8(isolate, file_path));

		if (me.HasCaught())
			throw Exception(me.Message());

		return script;
	}

	v8::Local<v8::Script> script;

	/**
	 * Load script from `file_path`
	 */
	inline
	ScriptFile(const char* file_path) throw (Exception):
		script(compile(context->GetIsolate(), file_path))
	{
		v8::Isolate* isolate = context->GetIsolate();
		v8::Local<v8::Object> global = context->Global();

		global->Set(v8::String::NewFromUtf8(isolate, "exports"),
		            v8::Object::New(isolate));
	}

	/**
	 * Run the script and catch errors.
	 */
	inline
	v8::Local<v8::Value> run() throw (Exception) {
		v8::TryCatch me;
		v8::Local<v8::Value> val = script->Run();

		if (me.HasCaught())
			throw Exception(me.Message());

		return val;
	}

	inline
	v8::Local<v8::Value> exports() {
		v8::Isolate* isolate = context->GetIsolate();
		v8::Local<v8::Object> global = context->Global();
		v8::Local<v8::String> export_key = v8::String::NewFromUtf8(isolate, "exports");

		if (global->Has(export_key)) {
			return global->Get(export_key);
		} else {
			return v8::Null(isolate);
		}
	}
};

}
}

#endif
