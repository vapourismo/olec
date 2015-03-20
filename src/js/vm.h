#ifndef OLEC_JS_VM_H_
#define OLEC_JS_VM_H_

#include <v8.h>

namespace olec {
namespace js {

/**
 * V8 Engine Instance
 */
struct EngineInstance {
	v8::Isolate* isolate;

	inline
	EngineInstance() {
		isolate = v8::Isolate::New();
		isolate->Enter();
	}

	inline
	~EngineInstance() {
		isolate->Exit();
		isolate->Dispose();
	}

	inline
	operator v8::Isolate*() {
		return isolate;
	}
};

}
}

#endif
