#ifndef OLEC_JS_VM_H_
#define OLEC_JS_VM_H_

#include <v8.h>

namespace olec {
namespace js {

/**
 * JavaScript Virtual Machine
 */
struct JavaScriptVM {
	v8::Isolate* isolate;

	inline
	JavaScriptVM() {
		v8::V8::InitializeICU();
		v8::V8::Initialize();

		isolate = v8::Isolate::New();
	}

	inline
	~JavaScriptVM() {
		isolate->Dispose();
		v8::V8::Dispose();
	}

	inline
	operator v8::Isolate*() {
		return isolate;
	}
};

}
}

#endif
