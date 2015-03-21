#include <v8.h>

using namespace v8;

struct _V8Initializer {
	_V8Initializer() {
		V8::InitializeICU();
		V8::Initialize();
	}

	~_V8Initializer() {
		V8::Dispose();
	}
} _v8init;
