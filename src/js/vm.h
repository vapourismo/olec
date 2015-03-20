#ifndef OLEC_JS_VM_H_
#define OLEC_JS_VM_H_

#include <v8.h>
#include <memory>

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

	inline
	EngineInstance():
		isolate(v8::Isolate::New()),
		isolate_scope(isolate.get()),
		handle_scope(isolate.get())
	{}

	inline
	operator v8::Isolate*() {
		return isolate.get();
	}
};

/**
 * Context
 */
struct Context: v8::Context::Scope {
	inline
	Context(v8::Isolate* isolate, v8::Handle<v8::ObjectTemplate> globals = v8::Handle<v8::ObjectTemplate>()):
		v8::Context::Scope(v8::Context::New(isolate, nullptr, globals))
	{}
};

}
}

#endif
