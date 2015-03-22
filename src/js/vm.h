#ifndef OLEC_JS_VM_H_
#define OLEC_JS_VM_H_

#include "tpls.h"

#include <v8.h>
#include <memory>
#include <map>
#include <fstream>
#include <string>

namespace olec {
namespace js {

// Forward declaration
struct EngineInstance;

/**
 * Context
 */
struct Context {
	v8::Local<v8::Context> context;
	v8::Context::Scope context_scope;

	inline
	Context(v8::Isolate* isolate):
		context(v8::Context::New(isolate, nullptr)),
		context_scope(context)
	{}

	inline
	Context(EngineInstance* vm);

	inline
	Context();
};

/**
 * V8 Engine Instance
 */
struct EngineInstance {
	struct IsolateDeleter {
		void operator ()(v8::Isolate* isolate) {
			isolate->Dispose();
		}
	};

	/**
	 * Initialize and dispose global V8 state
	 */
	struct _V8 {
		_V8() {
			v8::V8::InitializeICU();
			v8::V8::Initialize();
		}

		~_V8() {
			v8::V8::Dispose();
		}
	} _v8;

	std::unique_ptr<v8::Isolate, IsolateDeleter> isolate;
	v8::Isolate::Scope isolate_scope;
	v8::HandleScope handle_scope;
	Context global_context;

	ObjectTemplate global_template;
	std::map<String, v8::Persistent<v8::Value>> modules;

	EngineInstance();

	inline
	operator v8::Isolate*() const {
		return isolate.get();
	}

	inline
	v8::Isolate* get() const {
		return isolate.get();
	}

	static inline
	EngineInstance* current() {
		return (EngineInstance*) v8::Isolate::GetCurrent()->GetData(0);
	}
};

inline
Context::Context(EngineInstance* vm):
	context(v8::Context::New(vm->get(), nullptr, vm->global_template)),
	context_scope(context)
{}

inline
Context::Context():
	Context(EngineInstance::current())
{}

/**
 * Load files as an executable script
 */
struct ScriptFile: Context {
	static inline
	v8::Local<v8::String> read(v8::Isolate* isolate, const char* file_path) {
		std::string contents;
		std::ifstream source(file_path);

		if (source) {
			source.seekg(0, std::ios::end);
			contents.reserve(source.tellg());

			source.seekg(0, std::ios::beg);
			contents.assign(std::istreambuf_iterator<char>(source), std::istreambuf_iterator<char>());

			return v8::String::NewFromUtf8(isolate, contents.c_str());
		} else {
			std::string exc_message = "ScriptError: Cannot open file '" + std::string(file_path) + "'";
			isolate->ThrowException(v8::String::NewFromUtf8(isolate, exc_message.c_str()));
			return v8::Local<v8::String>();
		}
	}

	static inline
	v8::Local<v8::Script> compile(v8::Isolate* isolate, const char* file_path) {
		v8::Local<v8::String> contents = read(isolate, file_path);

		if (contents.IsEmpty()) {
			return v8::Local<v8::Script>();
		} else {
			return v8::Script::Compile(contents, Foreign<String>::generate(isolate, file_path));
		}
	}

	v8::Local<v8::Script> script;

	/**
	 * Load script from `file_path`
	 */
	inline
	ScriptFile(const char* file_path):
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
	v8::Local<v8::Value> run() {
		return script->Run();
	}
};

}
}

#endif
